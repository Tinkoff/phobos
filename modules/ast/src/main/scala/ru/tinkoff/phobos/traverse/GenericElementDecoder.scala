package ru.tinkoff.phobos.traverse

import cats.syntax.either._
import com.fasterxml.aalto.AsyncXMLStreamReader
import ru.tinkoff.phobos.ast.{Leaf, Node, XmlEntry}
import ru.tinkoff.phobos.decoding.{Cursor, DecodingError, ElementDecoder, TextDecoder}
import scala.annotation.tailrec
import scala.util.Try
import GenericElementDecoder.DecoderState

class GenericElementDecoder[Acc, Result] private (state: DecoderState, logic: TraversingLogic[Acc, Result])
    extends ElementDecoder[Result] {

  override def decodeAsElement(cursor: Cursor,
                               localName: String,
                               namespaceUri: Option[String]): ElementDecoder[Result] = {

    @tailrec
    def go(currentState: DecoderState, acc: Acc): ElementDecoder[Result] = {
      println(s"decoding entry name=$localName cursor=$cursor, state=$currentState")
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
                    XmlEntry.Text.attributeDecoder
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
            val newAcc: Acc = TextDecoder.stringDecoder
              .decodeAsText(cursor, localName, namespaceUri)
              .result(cursor.history)
              .map(parseLeaf)
              .map {
                case XmlEntry.Text("") => acc
                case other             => logic.onText(acc, localName, other)
              }
              .getOrElse(acc)

            if (cursor.isStartElement) {
              go(DecoderState.DecodingElement(cursor.getLocalName), newAcc)
            } else if (cursor.isEndElement) {
              if (cursor.getLocalName == localName) {
                cursor.next()
                new ElementDecoder.ConstDecoder(logic.onFinish(newAcc))
              } else {
                go(DecoderState.DecodingSelf, newAcc)
              }
            } else {
              cursor.next()
              go(DecoderState.DecodingSelf, newAcc)
            }

          case DecoderState.DecodingElement(field) =>
            val element = {
              if (cursor.isStartElement)
                decodeAsElement(cursor, field, None)
                  .map(logic.combine(acc, field, _))
              else
//                XmlEntry.Text.elementDecoder
//                  .decodeAsElement(cursor, field, None)
//                  .map(parseLeaf)
//                  .map {
//                    case XmlEntry.Text("") => acc
//                    case other             => logic.onText(acc, field, other)
//                  }
                new ElementDecoder.ConstDecoder(acc)
            }

            println(s"Element name=$field encoded as $element")
            if (element.isCompleted) {
              element.result(cursor.history) match {
                case Right(newAcc) =>
                  go(DecoderState.DecodingSelf, newAcc)
                case Left(error) => fail(error)
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

  private val parseLeaf: String => Leaf = {
    case "true"  => XmlEntry.True
    case "false" => XmlEntry.False
    case other =>
      Try(XmlEntry.IntNumber(other.toInt))
        .orElse(Try(XmlEntry.LongNumber(other.toLong)))
        .orElse(Try(XmlEntry.DoubleNumber(other.toDouble)))
        .getOrElse(XmlEntry.Text(other))
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
  def apply[Acc, Result](logic: TraversingLogic[Acc, Result]): GenericElementDecoder[Acc, Result] =
    new GenericElementDecoder(DecoderState.New, logic)

  sealed trait DecoderState
  object DecoderState {
    case object New                           extends DecoderState
    case object DecodingSelf                  extends DecoderState
    case class DecodingElement(field: String) extends DecoderState
  }
}
