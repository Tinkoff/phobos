package ru.tinkoff.phobos.ast

import ru.tinkoff.phobos.decoding.{AttributeDecoder, ElementDecoder}
import ru.tinkoff.phobos.encoding.{AttributeEncoder, ElementEncoder, TextEncoder}

sealed trait XmlEntry
object XmlEntry {

  sealed trait Leaf extends XmlEntry {
    private[phobos] type Self <: Leaf
    val companion: XmlLeafCompanion[Self]

    final type ScalaType = companion.ScalaType
    def value: ScalaType
  }

  sealed trait XmlLeafCompanion[E <: Leaf] {
    type ScalaType
    def apply(value: ScalaType): E

    private[phobos] val attributeEncoder: AttributeEncoder[ScalaType]
    private[phobos] val attributeDecoder: AttributeDecoder[ScalaType]

    private[phobos] val elementEncoder: ElementEncoder[ScalaType]
    private[phobos] val elementDecoder: ElementDecoder[ScalaType]

    private[phobos] val textEncoder: TextEncoder[ScalaType]
  }

  sealed trait Number extends Leaf

  object IntNumber extends XmlLeafCompanion[IntNumber] {
    final override type ScalaType = Int

    override private[phobos] val attributeEncoder = AttributeEncoder.intEncoder
    override private[phobos] val attributeDecoder = AttributeDecoder.intDecoder
    override private[phobos] val elementEncoder   = ElementEncoder.intEncoder
    override private[phobos] val elementDecoder   = ElementDecoder.intDecoder
    override private[phobos] val textEncoder      = TextEncoder.intEncoder
  }
  case class IntNumber(value: Int) extends Number {
    final override type Self = IntNumber
    final override val companion = IntNumber
  }

  object LongNumber extends XmlLeafCompanion[LongNumber] {
    final override type ScalaType = Long

    override private[phobos] val attributeEncoder = AttributeEncoder.longEncoder
    override private[phobos] val attributeDecoder = AttributeDecoder.longDecoder
    override private[phobos] val elementEncoder   = ElementEncoder.longEncoder
    override private[phobos] val elementDecoder   = ElementDecoder.longDecoder
    override private[phobos] val textEncoder      = TextEncoder.longEncoder
  }
  case class LongNumber(value: Long) extends Number {
    final override type Self = LongNumber
    final override val companion = LongNumber
  }

  object DoubleNumber extends XmlLeafCompanion[DoubleNumber] {
    final override type ScalaType = Double

    override private[phobos] val attributeEncoder = AttributeEncoder.doubleEncoder
    override private[phobos] val attributeDecoder = AttributeDecoder.doubleDecoder
    override private[phobos] val elementEncoder   = ElementEncoder.doubleEncoder
    override private[phobos] val elementDecoder   = ElementDecoder.doubleDecoder
    override private[phobos] val textEncoder      = TextEncoder.doubleEncoder
  }
  case class DoubleNumber(value: Double) extends Number {
    final override type Self = DoubleNumber
    final override val companion = DoubleNumber
  }

  object Text extends XmlLeafCompanion[Text] {
    final override type ScalaType = String

    override private[phobos] val attributeEncoder = AttributeEncoder.stringEncoder
    override private[phobos] val attributeDecoder = AttributeDecoder.stringDecoder
    override private[phobos] val elementEncoder   = ElementEncoder.stringEncoder
    override private[phobos] val elementDecoder   = ElementDecoder.stringDecoder
    override private[phobos] val textEncoder      = TextEncoder.stringEncoder
  }
  case class Text(value: String) extends Leaf {
    final override type Self = Text
    final override val companion = Text
  }

  object Bool extends XmlLeafCompanion[Bool] {
    final override type ScalaType = Boolean

    override private[phobos] val attributeEncoder = AttributeEncoder.booleanEncoder
    override private[phobos] val attributeDecoder = AttributeDecoder.booleanDecoder
    override private[phobos] val elementEncoder   = ElementEncoder.booleanEncoder
    override private[phobos] val elementDecoder   = ElementDecoder.booleanDecoder
    override private[phobos] val textEncoder      = TextEncoder.booleanEncoder

    override def apply(value: Boolean): Bool = if (value) True else False
  }
  sealed abstract class Bool(override val value: Boolean) extends Leaf {
    final override type Self = Bool
    final override val companion = Bool
  }

  case object True  extends Bool(true)
  case object False extends Bool(false)

  sealed trait Node extends XmlEntry {
    def attributes: List[(String, Leaf)]
  }
  case class TextNode(attributes: List[(String, Leaf)], text: Leaf) extends Node
  case class ParentNode(
      attributes: List[(String, Leaf)],
      children: List[(String, XmlEntry)]
  ) extends Node

  implicit val xmlEntryEncoder: ElementEncoder[XmlEntry] = impl.XmlEntryElementEncoder

  implicit val xmlEntryDecoder: ElementDecoder[XmlEntry] =
    traverse.GenericElementDecoder(traverse.AstTraversingLogic.instance)
}
