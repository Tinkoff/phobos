package ru.tinkoff.phobos

import ru.tinkoff.phobos.ast.impl.XmlBuildingBlock

package object ast {
  type XmlLeaf = XmlEntry.impl.Leaf

  type XmlNumber = XmlEntry.impl.Number
  object XmlNumber {
    type Aux[N] = XmlEntry.impl.Number { type ScalaType = N }

    def integral(value: Long): Aux[Long]   = XmlEntry.impl.IntegralNumber(value)
    def double(value: Double): Aux[Double] = XmlEntry.impl.DoubleNumber(value)

    def unapply[N](number: Aux[N]): Option[N] = Some(number.value)
  }

  type XmlText = XmlEntry.impl.Text
  val XmlText = XmlEntry.impl.Text

  type XmlNode = XmlEntry.impl.Node
  val XmlNode = XmlEntry.impl.Node

  type XmlBoolean = XmlEntry.impl.Bool
  object XmlBoolean {
    def fromBoolean(value: Boolean): XmlBoolean    = XmlEntry.impl.Bool(value)
    def unapply(bool: XmlBoolean): Option[Boolean] = Some(bool.value)

    val True  = XmlEntry.impl.True
    val False = XmlEntry.impl.False
  }

  implicit def byte2XmlIntegral(value: Byte): XmlNumber.Aux[Long]   = XmlNumber.integral(value)
  implicit def short2XmlIntegral(value: Short): XmlNumber.Aux[Long] = XmlNumber.integral(value)
  implicit def integer2XmlIntegral(value: Int): XmlNumber.Aux[Long] = XmlNumber.integral(value)
  implicit def long2XmlIntegral(value: Long): XmlNumber.Aux[Long]   = XmlNumber.integral(value)

  implicit def float2XmlDouble(value: Float): XmlNumber.Aux[Double]   = XmlNumber.double(value)
  implicit def double2XmlDouble(value: Double): XmlNumber.Aux[Double] = XmlNumber.double(value)

  implicit def boolean2XmlBool(value: Boolean): XmlBoolean = XmlBoolean.fromBoolean(value)
  implicit def string2XmlText(value: String): XmlText      = XmlText(value)
  implicit def character2XmlText(value: Char): XmlText     = string2XmlText(value.toString)

  object xml {
    val empty: XmlNode = XmlNode(Nil, Nil)

    def apply(more: XmlBuildingBlock*): XmlNode = empty(more: _*)
  }

  def node(name: String): impl.NodeName = new impl.NodeName(name)
  def attr(name: String): impl.AttrName = new impl.AttrName(name)
}
