package ru.tinkoff.phobos.ast.impl

import ru.tinkoff.phobos.ast._

sealed trait XmlBuildingBlock
class AttrName(private val name: String) extends AnyVal {

  /**
    * @param value - some [[XmlLeaf]]
    * @return - an attribute with given [[name]] and value
    * */
  def :=(value: XmlLeaf): Attr = new Attr(name, value)
}

class Attr(val name: String, val value: XmlLeaf) extends XmlBuildingBlock

class NodeName(private val name: String) extends AnyVal {

  /**
    * @param value - some XML part (either [[XmlLeaf]] or [[XmlNode]])
    * @return - an node with given [[name]] and value
    * */
  def :=(value: XmlEntry): NodePair = new NodePair(name, value)
}

class NodePair(val name: String, val value: XmlEntry) extends XmlBuildingBlock
