package ru.tinkoff.phobos

package object ast {
  type Leaf   = XmlEntry.Leaf
  type Number = XmlEntry.Number
  object Number {
    type Aux[N] = XmlEntry.Number { type ScalaType = N }
  }

  type Text = XmlEntry.Text
  type Node = XmlEntry.Node
  type Bool = XmlEntry.Bool
  object Bool {
    def fromBoolean(value: Boolean): Bool = XmlEntry.Bool(value)
  }

  implicit def integer2XmlNumber(value: Int): Number.Aux[Int]      = XmlEntry.IntNumber(value)
  implicit def long2XmlLong(value: Long): Number.Aux[Long]         = XmlEntry.LongNumber(value)
  implicit def double2XmlDouble(value: Double): Number.Aux[Double] = XmlEntry.DoubleNumber(value)
  implicit def boolean2XmlBool(value: Boolean): Bool               = Bool.fromBoolean(value)
  implicit def string2XmlText(value: String): Text                 = XmlEntry.Text(value)
  implicit def character2XmlText(value: Char): Text                = string2XmlText(value.toString)

  object XmlNode {
    class PartiallyApplied(private val attrs: Seq[(String, Leaf)]) extends AnyVal {
      def withChildren(xs: (String, XmlEntry)*): Node = XmlEntry.Node(
        attributes = attrs.toList,
        children = xs.toList
      )
    }

    def withAttributes(xs: (String, Leaf)*): PartiallyApplied = new PartiallyApplied(xs)

    def withChildren(xs: (String, XmlEntry)*): Node = withAttributes().withChildren(xs: _*)

    val empty: Node = withAttributes().withChildren()
  }
}
