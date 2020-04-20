package ru.tinkoff.phobos.traverse

import cats.syntax.either._
import com.fasterxml.aalto.AsyncXMLStreamReader
import ru.tinkoff.phobos.ast._
import ru.tinkoff.phobos.decoding.{Cursor, DecodingError, ElementDecoder, TextDecoder}
import scala.annotation.tailrec
import scala.util.Try
import GenericElementDecoder.DecoderState

class GenericElementDecoder[Acc, Result] private (state: DecoderState, logic: DecodingTraversingLogic[Acc, Result])
    extends ElementDecoder[Result] {

  override def decodeAsElement(cursor: Cursor,
                               localName: String,
                               namespaceUri: Option[String]): ElementDecoder[Result] = {

    @tailrec
    def go(currentState: DecoderState, acc: Acc): ElementDecoder[Result] = {
      println(s"decoding entry name=$localName history=${cursor.history.reverse
        .mkString("{ ", " -> ", " }")} state=$currentState acc = $acc")
      if (cursor.getEventType == AsyncXMLStreamReader.EVENT_INCOMPLETE) {
        cursor.next()
        new GenericElementDecoder(currentState, logic)
      } else
        currentState match {
          case DecoderState.New =>
            if (cursor.isStartElement) {
              val attributes = List
                .tabulate(cursor.getAttributeCount) { idx =>
                  val name = cursor.getAttributeName(idx).getLocalPart
                  name -> parseLeaf {
                    XmlText.attributeDecoder
                      .decodeAsAttribute(cursor, name, None)
                      .valueOr(throw _)
                  }
                }
              cursor.next()
              go(DecoderState.DecodingSelf, logic.onAttributes(acc, attributes))
            } else {
              fail(cursor, s"Illegal state: expected START_ELEMENT, got ${cursor.getEventType}")
            }

          case DecoderState.DecodingSelf =>
//            println(s"DECODING SELF localname=$localName cursor.getLocalName=${cursor.getLocalName}")
            val newAcc: Acc = {
              val tmp = TextDecoder.stringDecoder
                .decodeAsText(cursor, localName, namespaceUri)
                .result(cursor.history)
                .map(parseLeaf)
                .map {
                  case XmlText("") => acc
                  case other =>
                    logic.onText(acc, localName, other)
                }
              println(s"DECODING text FOR localname=$localName result=$tmp")
              tmp.getOrElse(acc)
            }

            val field = cursor.getLocalName
            println(s"DECODING SELF 2 localname=$localName cursor.getLocalName=$field")

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

  private implicit class Ops[U](self: ElementDecoder[XmlEntry]) {
    def orAttempt(that: => ElementDecoder[XmlEntry]): ElementDecoder[XmlEntry] =
      if (self.result(Nil).isRight) self
      else that
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

  override def result(history: List[String]): Either[DecodingError, Result] =
    Left(DecodingError("Decoding not complete", history))

  override val isCompleted: Boolean = false
}

object GenericElementDecoder {
  def apply[Acc, Result](logic: DecodingTraversingLogic[Acc, Result]): GenericElementDecoder[Acc, Result] =
    new GenericElementDecoder(DecoderState.New, logic)

  sealed trait DecoderState
  object DecoderState {
    case object New                           extends DecoderState
    case object DecodingSelf                  extends DecoderState
    case class DecodingElement(field: String) extends DecoderState
  }
}
