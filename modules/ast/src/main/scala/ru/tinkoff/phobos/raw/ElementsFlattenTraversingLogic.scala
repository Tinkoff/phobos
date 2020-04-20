package ru.tinkoff.phobos.raw

import ru.tinkoff.phobos.ast.XmlLeaf
import ru.tinkoff.phobos.raw.ElementsFlattenTraversingLogic.Accumulator
import ru.tinkoff.phobos.traverse.DecodingTraversingLogic
import scala.collection.mutable.ListBuffer

class ElementsFlattenTraversingLogic private () extends DecodingTraversingLogic[Accumulator, ElementsFlatten] {
  override def newAcc(): Accumulator = Accumulator()

  override def onFinish(acc: Accumulator): ElementsFlatten =
    ElementsFlatten(acc.elements.toList: _*)

  override def combine(acc: Accumulator, field: String, temporalResult: ElementsFlatten): Accumulator = {
    acc.elements ++= temporalResult.elems
    acc
  }

  override def onText(acc: Accumulator, elemName: String, text: XmlLeaf): Accumulator = {
    acc.elements += (elemName -> text)
    acc
  }
}

object ElementsFlattenTraversingLogic {
  case class Accumulator(elements: ListBuffer[(String, XmlLeaf)] = ListBuffer.empty)

  val instance = new ElementsFlattenTraversingLogic()
}
