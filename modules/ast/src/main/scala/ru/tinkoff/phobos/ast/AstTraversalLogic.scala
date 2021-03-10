package ru.tinkoff.phobos.ast

import ru.tinkoff.phobos.traverse.DecodingTraversalLogic
import AstTraversalLogic._
import ru.tinkoff.phobos.decoding.DecodingError
import scala.collection.mutable

class AstTraversalLogic private () extends DecodingTraversalLogic[Accumulator, XmlEntry] {
  override def newAcc(): Accumulator = Accumulator.ParentNode()

  override def onFinish(acc: Accumulator): XmlEntry = acc.toEntry

  override def onAttributes(acc: Accumulator, attributes: List[(String, XmlLeaf)]): Accumulator = {
    acc match {
      case acc: Accumulator.ParentNode =>
        acc.attributes ++= attributes
      case _ =>
    }
    acc
  }

  override def onText(acc: Accumulator, elemName: String, text: XmlLeaf): Accumulator = {
    acc match {
      case acc: Accumulator.ParentNode if acc.attributes.isEmpty && acc.children.isEmpty =>
        Accumulator.TextNode(text)

      case _ => acc
    }
  }

  override def combine(acc: Accumulator, field: String, intermediateResult: XmlEntry): Accumulator = {
    acc match {
      case acc: Accumulator.ParentNode =>
        acc.children += (field -> intermediateResult)
      case _ =>
    }
    acc
  }
}

object AstTraversalLogic {
  val instance: AstTraversalLogic = new AstTraversalLogic()

  type AttributeBuilder = mutable.ListBuffer[(String, XmlLeaf)]
  def AttributeBuilder(): AttributeBuilder = mutable.ListBuffer.empty

  type ChildrenBuilder = mutable.ListBuffer[(String, XmlEntry)]
  def ChildrenBuilder(): ChildrenBuilder = mutable.ListBuffer.empty

  sealed trait Accumulator {
    def toEntry: XmlEntry
  }

  object Accumulator {
    case class TextNode(toEntry: XmlLeaf) extends Accumulator

    case class ParentNode(
        attributes: AttributeBuilder = AttributeBuilder(),
        children: ChildrenBuilder = ChildrenBuilder(),
    ) extends Accumulator {

      override def toEntry: XmlNode = {
        XmlNode(attributes.toList, children.toList)
      }
    }

  }
}
