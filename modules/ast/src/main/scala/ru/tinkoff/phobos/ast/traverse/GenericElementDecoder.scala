package ru.tinkoff.phobos.ast.traverse

import cats.instances.list._
import cats.syntax.either._
import cats.syntax.traverse._
import com.fasterxml.aalto.AsyncXMLStreamReader
import ru.tinkoff.phobos.ast.impl.XmlEntryElementDecoder
import ru.tinkoff.phobos.ast.impl.XmlEntryElementDecoder.DecoderState
import ru.tinkoff.phobos.ast.{Leaf, Node, XmlEntry}
import ru.tinkoff.phobos.decoding.{Cursor, DecodingError, ElementDecoder, TextDecoder}
import scala.annotation.tailrec
import scala.util.Try

class GenericElementDecoder[Acc, Result] private (state: DecoderState, logic: TraversingLogic[Acc, Result])
    extends ElementDecoder[Result] {

  import XmlEntryElementDecoder._

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
            val parsedText = parseLeaf {
              TextDecoder.stringDecoder
                .decodeAsText(cursor, localName, namespaceUri)
                .result(cursor.history)
                .valueOr(throw _)
            }
            val newAcc = logic.onText(acc, parsedText)
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
              if (cursor.isStartElement) decodeAsElement(cursor, field, None)
              else
                XmlEntry.Text.elementDecoder.decodeAsElement(cursor, field, None).map(parseLeaf)
            }

            println(s"Element name=$field encoded as $element")
            if (element.isCompleted) {
              element.result(cursor.history) match {
                case Right(node: Node) =>
                  go(DecoderState.DecodingSelf, logic.onNode(acc, field, node))
                case Right(other) =>
                  fail(cursor, s"Expected $field no be xml node, got $other")
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
}
