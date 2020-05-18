package ru.tinkoff.phobos.traverse

import ru.tinkoff.phobos.ast.XmlLeaf

/**
  * Abstraction allowing to traverse arbitrary XML node(s).
  * Works with both mutable and immutable accumulators
  *
  * @tparam Acc - accumulator for traversal
  * @tparam Result - traversal result
  * */
trait DecodingTraversalLogic[Acc, Result] {

  /**
    * Starting point for traversal algorithm.
    *
    * @return - new accumulator for traversal
    * */
  def newAcc(): Acc

  /**
    * Updates accumulator with found attributes
    *
    * @param acc - accumulator
    * @param attributes - some XML node attributes
    * @return - updated accumulator
    * */
  def onAttributes(acc: Acc, attributes: List[(String, XmlLeaf)]): Acc = acc

  /**
    * Updates accumulator with found text
    *
    * @param acc - accumulator
    * @param elemName - name some XML element
    * @param text - decoded XML text (see [[XmlLeaf]] for details)
    * @return - updated accumulator
    * */
  def onText(acc: Acc, elemName: String, text: XmlLeaf): Acc = acc

  /**
    * Updates accumulator with intermediate traversal result
    *
    * @param acc - accumulator
    * @param field - name of decoded XML node
    * @param intermediateResult - result of recursive traversal
    * @return - updated accumulator
    * */
  def combine(acc: Acc, field: String, intermediateResult: Result): Acc

  /**
    * Creates the result of traversal algorithm based on accumulator.
    *
    * @param acc - accumulator
    * @return - traversal result
    * */
  def onFinish(acc: Acc): Result
}
