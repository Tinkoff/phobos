package ru.tinkoff.phobos.ast

import com.fasterxml.aalto.AsyncXMLStreamReader
import javax.xml.stream.XMLStreamConstants
import ru.tinkoff.phobos.decoding.ElementDecoder.{ConstDecoder, FailedDecoder, StringDecoder}
import ru.tinkoff.phobos.decoding.{Cursor, DecodingError, ElementDecoder}
import ru.tinkoff.phobos.encoding.{ElementEncoder, PhobosStreamWriter}
import scala.annotation.tailrec

sealed trait XmlEntry
object XmlEntry {

  object ast {

    sealed trait Leaf extends XmlEntry {
      type ScalaType

      def value: ScalaType

      protected final type LocalName = String
      protected final type AttrFunc  = PhobosStreamWriter => LocalName => Unit

      private[phobos] val elementEncoder: ElementEncoder[ScalaType]

      private[phobos] val attrFunc: AttrFunc
    }

    sealed trait Number extends Leaf

    case class IntNumber(value: Int) extends Number {
      final override type ScalaType = Int

      override private[phobos] val elementEncoder: ElementEncoder[Int] =
        ElementEncoder.intEncoder

      override private[phobos] val attrFunc: AttrFunc =
        sw => localName => sw.writeIntAttribute(prefix = "", namespaceURI = "", localName, value)
    }

    case class LongNumber(value: Long) extends Number {
      final override type ScalaType = Long

      override private[phobos] val elementEncoder: ElementEncoder[Long] =
        ElementEncoder.longEncoder

      override private[phobos] val attrFunc: AttrFunc =
        sw => localName => sw.writeLongAttribute(prefix = "", namespaceURI = "", localName, value)
    }

    case class DoubleNumber(value: Double) extends Number {
      final override type ScalaType = Double

      override private[phobos] val elementEncoder: ElementEncoder[Double] =
        ElementEncoder.doubleEncoder

      override private[phobos] val attrFunc: AttrFunc =
        sw => localName => sw.writeDoubleAttribute(prefix = "", namespaceURI = "", localName, value)
    }

    case class Text(value: String) extends Leaf {
      final override type ScalaType = String

      override private[phobos] val elementEncoder: ElementEncoder[String] =
        ElementEncoder.stringEncoder

      override private[phobos] val attrFunc: AttrFunc =
        sw => localName => sw.writeAttribute(localName, value)
    }

    sealed abstract class Bool(override val value: Boolean) extends Leaf {
      final override type ScalaType = Boolean

      override private[phobos] val elementEncoder: ElementEncoder[Boolean] =
        ElementEncoder.booleanEncoder

      override private[phobos] val attrFunc: AttrFunc =
        sw => localName => sw.writeBooleanAttribute(prefix = "", namespaceURI = "", localName, value)
    }

    case object True extends Bool(true)

    case object False extends Bool(false)

    case class Node(
        attributes: List[(String, Leaf)],
        children: List[(String, XmlEntry)]
    ) extends XmlEntry

  }

  implicit val xmlEntryEncoder: ElementEncoder[XmlEntry] = new ElementEncoder[XmlEntry] { self =>
    import XmlEntry.ast._
    override def encodeAsElement(
        entry: XmlEntry,
        sw: PhobosStreamWriter,
        localName: String,
        namespaceUri: Option[String]
    ): Unit = {
      entry match {
        case leaf: Leaf =>
          leaf.elementEncoder.encodeAsElement(leaf.value, sw, localName, namespaceUri)
        case Node(attributes, children) =>
          namespaceUri.fold(sw.writeStartElement(localName))(sw.writeStartElement(_, localName))
          attributes.foreach {
            case (attrName, attrValue) =>
              attrValue.attrFunc(sw)(attrName)
          }
          children foreach {
            case (childName, child) =>
              self.encodeAsElement(child, sw, childName, namespaceUri = None)
          }
          sw.writeEndElement()
      }
    }
  }

  class XmlEntryDecoder(current: Option[XmlEntry]) extends ElementDecoder[XmlEntry] {

    import XmlEntry.ast._

    override def decodeAsElement(c: Cursor,
                                 localName: String,
                                 namespaceUri: Option[String]): ElementDecoder[XmlEntry] = {

      ???
    }

    override def result(history: List[String]): Either[DecodingError, XmlEntry] =
      Left(DecodingError("Decoding not complete", history))

    override val isCompleted: Boolean = false
  }

  implicit val xmlEntryDecoder: ElementDecoder[XmlEntry] = new XmlEntryDecoder(None)
}
