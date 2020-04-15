package ru.tinkoff.phobos.raw

import ru.tinkoff.phobos.ast.Leaf
import ru.tinkoff.phobos.raw.ElementsFlattenTraversingLogic.Accumulator
import ru.tinkoff.phobos.traverse.TraversingLogic
import scala.collection.mutable.ListBuffer

class ElementsFlattenTraversingLogic private () extends TraversingLogic[Accumulator, ElementsFlatten] {
  override def newAcc(): Accumulator = Accumulator()

  override def onFinish(acc: Accumulator): ElementsFlatten =
    ElementsFlatten(acc.elements.toList: _*)

  override def combine(acc: Accumulator, field: String, temporalResult: ElementsFlatten): Accumulator = {
    acc.elements ++= temporalResult.elems
    acc
  }

  override def onText(acc: Accumulator, elemName: String, text: Leaf): Accumulator = {
    acc.elements += (elemName -> text)
    acc
  }
}

object ElementsFlattenTraversingLogic {
  case class Accumulator(elements: ListBuffer[(String, Leaf)] = ListBuffer.empty)

  val instance = new ElementsFlattenTraversingLogic()
}
