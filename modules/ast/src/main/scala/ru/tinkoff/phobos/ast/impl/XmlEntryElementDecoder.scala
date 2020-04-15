package ru.tinkoff.phobos.ast.impl

import com.fasterxml.aalto.AsyncXMLStreamReader
import ru.tinkoff.phobos.ast.XmlEntry.XmlLeafCompanion
import ru.tinkoff.phobos.ast._
import ru.tinkoff.phobos.decoding.{Cursor, DecodingError, ElementDecoder, TextDecoder}

import scala.annotation.tailrec
import XmlEntryElementDecoder.DecoderState
import javax.xml.stream.XMLStreamConstants
import cats.instances.either._
import cats.instances.list._
import cats.syntax.traverse._
import scala.util.Try

class XmlEntryElementDecoder private (state: DecoderState) extends ElementDecoder[XmlEntry] {
  import XmlEntryElementDecoder._
  override def decodeAsElement(cursor: Cursor,
                               localName: String,
                               namespaceUri: Option[String]): ElementDecoder[XmlEntry] = {

    @tailrec
    def go(currentState: DecoderState, acc: Accumulator): ElementDecoder[XmlEntry] = {
      println(s"decoding entry name=$localName cursor=$cursor, state=$currentState")
      if (cursor.getEventType == AsyncXMLStreamReader.EVENT_INCOMPLETE) {
        cursor.next()
        new XmlEntryElementDecoder(currentState)
      } else
        currentState match {
          case DecoderState.New =>
            if (cursor.isStartElement) {
              val attributesEither = List
                .range(0, cursor.getAttributeCount)
                .traverse[({ type L[+A] = Either[DecodingError, A] })#L, (String, Leaf)] { idx =>
                  val name = cursor.getAttributeName(idx).getLocalPart
                  XmlEntry.Text.attributeDecoder
                    .decodeAsAttribute(cursor, name, None)
                    .map(parseLeaf andThen name.->)
                }
              attributesEither match {
                case Right(attributes) =>
                  cursor.next()
                  acc.attributes ++= attributes
                  go(DecoderState.DecodingSelf, acc)

                case Left(error) => fail(error)
              }
            } else {
              fail(cursor, s"Illegal state: expected START_ELEMENT, got ${cursor.getEventType}")
            }

          case DecoderState.DecodingSelf =>
            val textDecoder = TextDecoder.stringDecoder
              .decodeAsText(cursor, localName, namespaceUri)
              .map {
                case ""  => None
                case str => Some(parseLeaf(str))
              }
            println(s"textDecoder=$textDecoder")
            textDecoder.result(cursor.history) match {
              case Right(textOpt) => acc.text = textOpt
              case _              =>
            }
            if (cursor.isStartElement) {
              go(DecoderState.DecodingElement(cursor.getLocalName), acc)
            } else if (cursor.isEndElement) {
              if (cursor.getLocalName == localName) {
                cursor.next()
                new ElementDecoder.ConstDecoder(acc.toNode)
              } else {
                go(DecoderState.DecodingSelf, acc)
              }
            } else {
              cursor.next()
              go(DecoderState.DecodingSelf, acc)
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
                  acc.children += (field -> node)
                  go(DecoderState.DecodingSelf, acc)
                case Right(leaf: Leaf) =>
                  fail(cursor, s"Expected $field no be xml node, got $leaf")
                case Left(error) => fail(error)
              }
            } else {
              go(DecoderState.DecodingElement(field), acc)
            }
        }
    }

    go(state, Accumulator())
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

  override def result(history: List[String]): Either[DecodingError, XmlEntry] =
    Left(DecodingError("Decoding not complete", history))

  override val isCompleted: Boolean = false
}

object XmlEntryElementDecoder {
  import scala.collection.mutable
  type AttributeBuilder = mutable.ListBuffer[(String, Leaf)]
  def AttributeBuilder(): AttributeBuilder = mutable.ListBuffer.empty

  type ChildrenBuilder = mutable.ListBuffer[(String, Node)]
  def ChildrenBuilder(): ChildrenBuilder = mutable.ListBuffer.empty

  type TextBuilder = Option[Leaf]
  def TextBuilder(): TextBuilder = None

  case class Accumulator(attributes: AttributeBuilder = AttributeBuilder(),
                         children: ChildrenBuilder = ChildrenBuilder(),
                         var text: TextBuilder = TextBuilder()) {

    def toNode: XmlEntry.Node = {
      val partiallyApplied = XmlNode.withAttributes(attributes.toList: _*)
      text.fold(ifEmpty = partiallyApplied.withChildren(children.toList: _*))(partiallyApplied.withText)
    }
  }

  sealed trait DecoderState
  object DecoderState {
    case object New extends DecoderState

    case object DecodingSelf extends DecoderState

    case class DecodingElement(field: String) extends DecoderState
  }
  def apply(): XmlEntryElementDecoder = new XmlEntryElementDecoder(DecoderState.New)
}
