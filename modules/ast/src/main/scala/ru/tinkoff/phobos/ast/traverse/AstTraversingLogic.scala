package ru.tinkoff.phobos.ast.traverse

import ru.tinkoff.phobos.ast._
import AstTraversingLogic._

class AstTraversingLogic private () extends TraversingLogic[Accumulator, XmlEntry] {
  override def newAcc(): Accumulator = Accumulator()

  override def onFinish(acc: Accumulator): XmlEntry = acc.toNode

  override def onAttributes(acc: Accumulator, attributes: List[(String, Leaf)]): Accumulator = {
    acc.attributes ++= attributes
    acc
  }

  override def onText(acc: Accumulator, text: Leaf): Accumulator = {
    acc.text = text match {
      case XmlEntry.Text("") => None
      case other             => Some(other)
    }
    acc
  }

  override def onNode(acc: Accumulator, field: String, node: Node): Accumulator = {
    acc.children += (field -> node)
    acc
  }
}

object AstTraversingLogic {
  import scala.collection.mutable
  type AttributeBuilder = mutable.ListBuffer[(String, Leaf)]
  def AttributeBuilder(): AttributeBuilder = mutable.ListBuffer.empty

  type ChildrenBuilder = mutable.ListBuffer[(String, Node)]
  def ChildrenBuilder(): ChildrenBuilder = mutable.ListBuffer.empty

  type TextBuilder = Option[Leaf]
  def TextBuilder(): TextBuilder = None

  case class Accumulator(attributes: AttributeBuilder = AttributeBuilder(),
                         children: ChildrenBuilder = ChildrenBuilder(),
                         var text: TextBuilder = TextBuilder()) {

    def toNode: XmlEntry.Node = {
      val partiallyApplied = XmlNode.withAttributes(attributes.toList: _*)
      text.fold(ifEmpty = partiallyApplied.withChildren(children.toList: _*))(partiallyApplied.withText)
    }
  }

  val instance: AstTraversingLogic = new AstTraversingLogic()
}
