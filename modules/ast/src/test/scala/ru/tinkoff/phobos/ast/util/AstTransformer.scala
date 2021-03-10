package ru.tinkoff.phobos.ast.util

import ru.tinkoff.phobos.ast._

object AstTransformer {
  def sortNodeValues(xmlEntry: XmlEntry): XmlEntry = xmlEntry match {
    case XmlNode(attrs, children) =>
      val newAttrs = attrs.sortBy(_._1)
      val newChildren = children.map { case (n, v) =>
        n -> sortNodeValues(v)
      }.sortBy(_._1)
      XmlNode(newAttrs, newChildren)

    case _ => xmlEntry
  }
}
