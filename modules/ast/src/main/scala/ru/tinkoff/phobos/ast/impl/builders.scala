package ru.tinkoff.phobos.ast.impl

import ru.tinkoff.phobos.ast._

sealed trait XmlBuildingBlock
class AttrName(private val name: String) extends AnyVal {
  def :=(value: XmlLeaf): Attr = new Attr(name, value)
}

class Attr(val name: String, val value: XmlLeaf) extends XmlBuildingBlock

class NodeName(private val name: String) extends AnyVal {
  def :=(value: XmlEntry): NodePair = new NodePair(name, value)
}

class NodePair(val name: String, val value: XmlEntry) extends XmlBuildingBlock
