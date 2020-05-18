package ru.tinkoff.phobos.raw

import ru.tinkoff.phobos.ast._
import ru.tinkoff.phobos.decoding.ElementDecoder
import ru.tinkoff.phobos.traverse.GenericElementDecoder

/**
  * Data type containing pairs of the element name and it's text.
  * */
case class ElementsFlatten(elems: (String, XmlLeaf)*)

object ElementsFlatten {

  /**
    * Allows to decode [[ElementsFlatten]] from arbitrary XML node
    * */
  implicit val elementsFlattenDecoder: ElementDecoder[ElementsFlatten] =
    GenericElementDecoder(ElementsFlattenTraversalLogic.instance)
}
