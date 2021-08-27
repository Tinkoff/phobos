package ru.tinkoff.phobos.traverse

import cats.syntax.traverse._
import cats.instances.either._
import cats.instances.list._
import com.fasterxml.aalto.AsyncXMLStreamReader
import ru.tinkoff.phobos.ast._
import ru.tinkoff.phobos.decoding.{Cursor, DecodingError, ElementDecoder, TextDecoder}
import scala.annotation.tailrec
import scala.util.Try
import GenericElementDecoder.DecoderState

/** An encoder allowing to arbitrarily encode a XML element with provided traversal logic.
  *
  * @tparam Acc
  *   - accumulator
  * @tparam Result
  *   - decoding result
  * @param logic
  *   - see [[DecodingTraversalLogic]]
  */
class GenericElementDecoder[Acc, Result] private (state: DecoderState, logic: DecodingTraversalLogic[Acc, Result])
    extends ElementDecoder[Result] {

  override def decodeAsElement(
      cursor: Cursor,
      localName: String,
      namespaceUri: Option[String],
  ): ElementDecoder[Result] = {

    @tailrec
    def go(currentState: DecoderState, acc: Acc): ElementDecoder[Result] = {
      if (cursor.getEventType == AsyncXMLStreamReader.EVENT_INCOMPLETE) {
        cursor.next()
        new GenericElementDecoder(currentState, logic)
      } else
        currentState match {
          case DecoderState.New =>
            if (cursor.isStartElement) {
              val attributesEither = List
                .tabulate(cursor.getAttributeCount) { idx =>
                  val name = cursor.getAttributeName(idx).getLocalPart
                  XmlText.attributeDecoder
                    .decodeAsAttribute(cursor, name, None)
                    .map(str => name -> parseLeaf(str))
                }
                .sequence
              attributesEither match {
                case Right(attrs) =>
                  cursor.next()
                  go(DecoderState.DecodingSelf, logic.onAttributes(acc, attrs))
                case Left(e) =>
                  fail(e)
              }
            } else {
              fail(cursor, s"Illegal state: expected START_ELEMENT, got ${cursor.getEventType}")
            }

          case DecoderState.DecodingSelf =>
            val newAcc: Acc =
              TextDecoder.stringDecoder
                .decodeAsText(cursor)
                .result(cursor.history)
                .map(parseLeaf)
                .map {
                  case XmlText("") => acc
                  case other =>
                    logic.onText(acc, localName, other)
                }
                .getOrElse(acc)

            val field = cursor.getLocalName

            if (cursor.isStartElement) {
              val element = decodeAsElement(cursor, field, None)
              if (element.isCompleted) {
                element.result(cursor.history) match {
                  case Right(result) => go(DecoderState.DecodingSelf, logic.combine(newAcc, field, result))
                  case Left(error)   => fail(error)
                }
              } else {
                go(DecoderState.DecodingElement(field), newAcc)
              }
            } else if (cursor.isEndElement) {
              if (field == localName) {
                cursor.next()
                new ElementDecoder.ConstDecoder(logic.onFinish(newAcc))
              } else {
                cursor.next()
                go(DecoderState.DecodingSelf, newAcc)
              }
            } else {
              cursor.next()
              go(DecoderState.DecodingSelf, newAcc)
            }

          case DecoderState.DecodingElement(field) =>
            val element = decodeAsElement(cursor, field, None)
            if (element.isCompleted) {
              element.result(cursor.history) match {
                case Right(result) => go(DecoderState.DecodingSelf, logic.combine(acc, field, result))
                case Left(error)   => fail(error)
              }
            } else {
              go(DecoderState.DecodingElement(field), acc)
            }
        }
    }

    go(state, logic.newAcc())
  }

  private val parseLeaf: String => XmlLeaf = {
    case "true"  => XmlBoolean.True
    case "false" => XmlBoolean.False
    case other =>
      Try(XmlNumber.integral(other.toLong))
        .orElse(Try(XmlNumber.double(other.toDouble)))
        .getOrElse(XmlText(other))
  }

  private def fail[A](error: DecodingError): ElementDecoder[A] =
    new ElementDecoder.FailedDecoder[A](error)

  private def fail[A](cursor: Cursor, message: String): ElementDecoder[A] = {
    fail(cursor.error(message))
  }

  override def result(history: => List[String]): Either[DecodingError, Result] =
    Left(ElementDecoder.decodingNotCompleteError(history))

  override val isCompleted: Boolean = false
}

object GenericElementDecoder {

  /** Allows to create a [[GenericElementDecoder]] based on providing traversal logic
    *
    * @tparam Acc
    *   - accumulator
    * @tparam Result
    *   - decoding result
    * @param logic
    *   - see [[DecodingTraversalLogic]]
    * @return
    *   - an [[ElementDecoder]] for [[Result]]
    */
  def apply[Acc, Result](logic: DecodingTraversalLogic[Acc, Result]): ElementDecoder[Result] =
    new GenericElementDecoder(DecoderState.New, logic)

  sealed trait DecoderState
  object DecoderState {
    case object New                           extends DecoderState
    case object DecodingSelf                  extends DecoderState
    case class DecodingElement(field: String) extends DecoderState
  }
}
