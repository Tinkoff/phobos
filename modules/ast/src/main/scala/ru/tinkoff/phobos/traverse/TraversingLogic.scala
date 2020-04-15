package ru.tinkoff.phobos.traverse

import ru.tinkoff.phobos.ast.Leaf

trait TraversingLogic[Acc, Result] {
  def newAcc(): Acc

  def onAttributes(acc: Acc, attributes: List[(String, Leaf)]): Acc = acc

  def onText(acc: Acc, elemName: String, text: Leaf): Acc = acc

  def combine(acc: Acc, field: String, temporalResult: Result): Acc = acc

  def onFinish(acc: Acc): Result
}
