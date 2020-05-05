package ru.tinkoff.phobos.traverse

import ru.tinkoff.phobos.ast.XmlLeaf

trait DecodingTraversingLogic[Acc, Result] {
  def newAcc(): Acc

  def onAttributes(acc: Acc, attributes: List[(String, XmlLeaf)]): Acc = acc

  def onText(acc: Acc, elemName: String, text: XmlLeaf): Acc = acc

  def combine(acc: Acc, field: String, temporalResult: Result): Acc

  def onFinish(acc: Acc): Result
}
