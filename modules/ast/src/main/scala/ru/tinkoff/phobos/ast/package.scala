package ru.tinkoff.phobos

import ru.tinkoff.phobos.ast.impl.XmlBuildingBlock

package object ast {

  /** Base type for [[XmlNumber]], [[XmlBoolean]] and [[XmlText]] */
  type XmlLeaf = XmlEntry.impl.Leaf

  /** Encapsulates either real or integral number */
  type XmlNumber = XmlEntry.impl.Number
  object XmlNumber {
    type Aux[N] = XmlEntry.impl.Number { type ScalaType = N }

    /** Creates a XML leaf containing Long
      *
      * @param value - the number
      * @return - typed XML leaf
      */
    def integral(value: Long): Aux[Long] = XmlEntry.impl.IntegralNumber(value)

    /** Creates a XML leaf containing Double
      *
      * @param value - the number
      * @return - typed XML leaf
      */
    def double(value: Double): Aux[Double] = XmlEntry.impl.DoubleNumber(value)

    def unapply[N](number: Aux[N]): Option[N] = Some(number.value)
  }

  type XmlText = XmlEntry.impl.Text
  val XmlText = XmlEntry.impl.Text

  type XmlNode = XmlEntry.impl.Node
  val XmlNode = XmlEntry.impl.Node

  type XmlBoolean = XmlEntry.impl.Bool
  object XmlBoolean {

    /** Creates a XML leaf containing Boolean
      *
      * @param value - the boolean
      * @return - typed XML leaf
      */
    def fromBoolean(value: Boolean): XmlBoolean    = XmlEntry.impl.Bool(value)
    def unapply(bool: XmlBoolean): Option[Boolean] = Some(bool.value)

    val True  = XmlEntry.impl.True
    val False = XmlEntry.impl.False
  }

  object xml {

    /** Empty XML node */
    val empty: XmlNode = XmlNode(Nil, Nil)

    /** Starting point for building a XML document
      *
      * @param more - child nodes
      * @return - new XML node with given children
      */
    def apply(more: XmlBuildingBlock*): XmlNode = empty(more: _*)
  }

  /** Starting point for building a XML element
    *
    * @param name - the name of XML element
    */
  def node(name: String): impl.NodeName = new impl.NodeName(name)

  /** Starting point for building a XML attribute
    *
    * @param name - the name of XML attribute
    */
  def attr(name: String): impl.AttrName = new impl.AttrName(name)

  // useful implicit conversions for AST construction
  implicit def byte2XmlIntegral(value: Byte): XmlNumber.Aux[Long]   = XmlNumber.integral(value)
  implicit def short2XmlIntegral(value: Short): XmlNumber.Aux[Long] = XmlNumber.integral(value)
  implicit def integer2XmlIntegral(value: Int): XmlNumber.Aux[Long] = XmlNumber.integral(value)
  implicit def long2XmlIntegral(value: Long): XmlNumber.Aux[Long]   = XmlNumber.integral(value)

  implicit def float2XmlDouble(value: Float): XmlNumber.Aux[Double]   = XmlNumber.double(value)
  implicit def double2XmlDouble(value: Double): XmlNumber.Aux[Double] = XmlNumber.double(value)

  implicit def boolean2XmlBool(value: Boolean): XmlBoolean = XmlBoolean.fromBoolean(value)
  implicit def string2XmlText(value: String): XmlText      = XmlText(value)
  implicit def character2XmlText(value: Char): XmlText     = string2XmlText(value.toString)
}
