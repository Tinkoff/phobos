package ru.tinkoff.phobos.ast

import ru.tinkoff.phobos.ast.impl.{Attr, NodePair, XmlBuildingBlock}
import ru.tinkoff.phobos.decoding.{AttributeDecoder, ElementDecoder}
import ru.tinkoff.phobos.encoding.{AttributeEncoder, ElementEncoder, TextEncoder}
import ru.tinkoff.phobos.traverse.GenericElementDecoder

/**
  * Base type for XML nodes, elements and attributes values
  * */
sealed trait XmlEntry
object XmlEntry extends impl.CatsInstances {

  object impl {

    sealed trait Leaf extends XmlEntry {
      private[phobos] type Self <: Leaf
      type ScalaType

      val companion: XmlLeafCompanion[Self, ScalaType]
      def value: ScalaType
    }

    sealed trait XmlLeafCompanion[E <: Leaf, ScalaType] {
      def apply(value: ScalaType): E

      private[phobos] val attributeEncoder: AttributeEncoder[ScalaType]
      private[phobos] val attributeDecoder: AttributeDecoder[ScalaType]

      private[phobos] val elementEncoder: ElementEncoder[ScalaType]
      private[phobos] val elementDecoder: ElementDecoder[ScalaType]

      private[phobos] val textEncoder: TextEncoder[ScalaType]
    }

    sealed trait Number extends Leaf

    object IntegralNumber extends XmlLeafCompanion[IntegralNumber, Long] {
      override private[phobos] val attributeEncoder = AttributeEncoder.longEncoder
      override private[phobos] val attributeDecoder = AttributeDecoder.longDecoder
      override private[phobos] val elementEncoder   = ElementEncoder.longEncoder
      override private[phobos] val elementDecoder   = ElementDecoder.longDecoder
      override private[phobos] val textEncoder      = TextEncoder.longEncoder
    }

    final case class IntegralNumber(value: Long) extends Number {
      override type Self      = IntegralNumber
      override type ScalaType = Long
      override val companion = IntegralNumber
    }

    object DoubleNumber extends XmlLeafCompanion[DoubleNumber, Double] {
      override private[phobos] val attributeEncoder = AttributeEncoder.doubleEncoder
      override private[phobos] val attributeDecoder = AttributeDecoder.doubleDecoder
      override private[phobos] val elementEncoder   = ElementEncoder.doubleEncoder
      override private[phobos] val elementDecoder   = ElementDecoder.doubleDecoder
      override private[phobos] val textEncoder      = TextEncoder.doubleEncoder
    }

    final case class DoubleNumber(value: Double) extends Number {
      override type Self      = DoubleNumber
      override type ScalaType = Double
      override val companion = DoubleNumber
    }

    object Text extends XmlLeafCompanion[Text, String] {
      override private[phobos] val attributeEncoder = AttributeEncoder.stringEncoder
      override private[phobos] val attributeDecoder = AttributeDecoder.stringDecoder
      override private[phobos] val elementEncoder   = ElementEncoder.stringEncoder
      override private[phobos] val elementDecoder   = ElementDecoder.stringDecoder
      override private[phobos] val textEncoder      = TextEncoder.stringEncoder
    }

    final case class Text(value: String) extends Leaf {
      override type Self      = Text
      override type ScalaType = String
      override val companion = Text
    }

    object Bool extends XmlLeafCompanion[Bool, Boolean] {
      override private[phobos] val attributeEncoder = AttributeEncoder.booleanEncoder
      override private[phobos] val attributeDecoder = AttributeDecoder.booleanDecoder
      override private[phobos] val elementEncoder   = ElementEncoder.booleanEncoder
      override private[phobos] val elementDecoder   = ElementDecoder.booleanDecoder
      override private[phobos] val textEncoder      = TextEncoder.booleanEncoder

      override def apply(value: Boolean): Bool = if (value) True else False
    }

    sealed abstract class Bool(override val value: Boolean) extends Leaf {
      final override type Self      = Bool
      final override type ScalaType = Boolean
      final override val companion = Bool
    }

    case object True extends Bool(true)

    case object False extends Bool(false)

    final case class Node(
        attributes: List[(String, Leaf)],
        children: List[(String, XmlEntry)]
    ) extends XmlEntry {

      /**
        * @param more - new attributes and child nodes
        * @return - new [[XmlNode]] with given attributes and children added
        * */
      def apply(more: XmlBuildingBlock*): Node = {
        val attrs = more.collect {
          case attr: Attr => attr.name -> attr.value
        }.toList
        val children2 = more.collect {
          case node: NodePair => node.name -> node.value
        }.toList

        Node(attributes ++ attrs, children ++ children2)
      }
    }

  }

  implicit val xmlEntryEncoder: ElementEncoder[XmlEntry] = XmlEntryElementEncoder

  implicit val xmlEntryDecoder: ElementDecoder[XmlEntry] =
    GenericElementDecoder(AstTraversalLogic.instance)
}
