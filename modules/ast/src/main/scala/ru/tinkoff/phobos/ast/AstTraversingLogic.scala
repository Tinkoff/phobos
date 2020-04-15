package ru.tinkoff.phobos.ast

import ru.tinkoff.phobos.traverse.TraversingLogic
import AstTraversingLogic._
import scala.collection.mutable

class AstTraversingLogic private () extends TraversingLogic[Accumulator, XmlEntry] {
  override def newAcc(): Accumulator = Accumulator.ParentNode()

  override def onFinish(acc: Accumulator): XmlEntry = acc.toEntry

  override def onAttributes(acc: Accumulator, attributes: List[(String, Leaf)]): Accumulator = {
    acc match {
      case acc: Accumulator.ParentNode =>
        acc.attributes ++= attributes
      case _ =>
    }
    acc
  }

  override def onText(acc: Accumulator, elemName: String, text: Leaf): Accumulator = {
    acc match {
      case acc: Accumulator.TextNode =>
        acc.text = Some(text)
        acc
      case _ => Accumulator.TextNode(Some(text))
    }
  }

  override def combine(acc: Accumulator, field: String, entry: XmlEntry): Accumulator = {
    acc match {
      case acc: Accumulator.ParentNode =>
        acc.children += (field -> entry)
      case _ =>
    }
    acc
  }
}

object AstTraversingLogic {
  val instance: AstTraversingLogic = new AstTraversingLogic()

  type AttributeBuilder = mutable.ListBuffer[(String, Leaf)]
  def AttributeBuilder(): AttributeBuilder = mutable.ListBuffer.empty

  type ChildrenBuilder = mutable.ListBuffer[(String, XmlEntry)]
  def ChildrenBuilder(): ChildrenBuilder = mutable.ListBuffer.empty

  sealed trait Accumulator {
    def toEntry: XmlEntry
  }
  object Accumulator {
    case class TextNode(var text: Option[Leaf] = None) extends Accumulator {

      override def toEntry: Leaf = text.getOrElse(???)
    }

    case class ParentNode(attributes: AttributeBuilder = AttributeBuilder(),
                          children: ChildrenBuilder = ChildrenBuilder())
        extends Accumulator {

      override def toEntry: XmlEntry.Node = {
        XmlNode.withAttributes(attributes.toList: _*).withChildren(children.toList: _*)
      }
    }

  }
}
