package ru.tinkoff.phobos.raw

import ru.tinkoff.phobos.ast._
import ru.tinkoff.phobos.decoding.ElementDecoder
import ru.tinkoff.phobos.traverse.GenericElementDecoder

case class ElementsFlatten(elems: (String, Leaf)*)

object ElementsFlatten {
  implicit val elementsFlattenDecoder: ElementDecoder[ElementsFlatten] =
    GenericElementDecoder(ElementsFlattenTraversingLogic.instance)
}
