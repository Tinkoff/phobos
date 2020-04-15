package ru.tinkoff.phobos.ast.traverse

import ru.tinkoff.phobos.ast._

trait TraversingLogic[Acc, Result] {
  def newAcc(): Acc

  def onAttributes(acc: Acc, attributes: List[(String, Leaf)]): Acc = acc

  def onText(acc: Acc, text: Leaf): Acc = acc

  def onNode(acc: Acc, field: String, node: Node): Acc = acc

  def onFinish(acc: Acc): Result
}
