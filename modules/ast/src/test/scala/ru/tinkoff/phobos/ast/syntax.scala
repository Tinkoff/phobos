package ru.tinkoff.phobos.ast

import XmlEntry.ast

object syntax {

  type Leaf   = ast.Leaf
  type Number = ast.Number
  object Number {
    type Aux[N] = ast.Number { type ScalaType = N }
  }

  type Text = ast.Text
  type Node = ast.Node
  type Bool = ast.Bool

  implicit def integer(value: Int): Number.Aux[Int]      = ast.IntNumber(value)
  implicit def long(value: Long): Number.Aux[Long]       = ast.LongNumber(value)
  implicit def double(value: Double): Number.Aux[Double] = ast.DoubleNumber(value)
  implicit def boolean(value: Boolean): Bool             = if (value) ast.True else ast.False
  implicit def text(value: String): Text                 = ast.Text(value)
  implicit def character(value: Char): Text              = text(value.toString)

  object XmlNode {
    class PartiallyApplied(private val attrs: Seq[(String, Leaf)]) extends AnyVal {
      def children(xs: (String, XmlEntry)*): Node = ast.Node(
        attributes = attrs.toList,
        children = xs.toList
      )
    }

    val withoutAttributes: PartiallyApplied               = attributes()
    def attributes(xs: (String, Leaf)*): PartiallyApplied = new PartiallyApplied(xs)

    val empty: Node = withoutAttributes.children()
  }
}
