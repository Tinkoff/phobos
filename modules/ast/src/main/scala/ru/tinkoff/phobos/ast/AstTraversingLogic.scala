package ru.tinkoff.phobos.ast

import ru.tinkoff.phobos.traverse.DecodingTraversingLogic
import AstTraversingLogic._
import ru.tinkoff.phobos.decoding.DecodingError
import scala.collection.mutable

class AstTraversingLogic private () extends DecodingTraversingLogic[Accumulator, XmlEntry] {
  override def newAcc(): Accumulator = Accumulator.ParentNode()

  override def onFinish(acc: Accumulator): XmlEntry = acc.toEntry

  override def onAttributes(acc: Accumulator, attributes: List[(String, XmlLeaf)]): Accumulator = {
    acc match {
      case acc: Accumulator.ParentNode =>
        acc.attributes ++= attributes
      case other =>
        println(s"WARN ignoring attrs for $other")
    }
    acc
  }

  override def onText(acc: Accumulator, elemName: String, text: XmlLeaf): Accumulator = {
    acc match {
      case acc: Accumulator.TextNode =>
        acc.text = Some(text)
        acc

      case acc: Accumulator.ParentNode if acc.attributes.isEmpty && acc.children.isEmpty =>
        Accumulator.TextNode(Some(text))
//        Accumulator.ParentNode(children = ChildrenBuilder() += (elemName -> text))

      case acc: Accumulator.ParentNode =>
        println(s"[WARN] Got $acc while having $text name=$elemName")
        acc.children += (elemName -> text)
        acc
    }
  }

  override def combine(acc: Accumulator, field: String, entry: XmlEntry): Accumulator = {
    acc match {
      case acc: Accumulator.ParentNode =>
        acc.children += (field -> entry)
      case _ =>
        println(s"[WARN] don't now how to combine $acc with name=$field entry=$entry")
    }
    acc
  }
}

object AstTraversingLogic {
  val instance: AstTraversingLogic = new AstTraversingLogic()

  type AttributeBuilder = mutable.ListBuffer[(String, XmlLeaf)]
  def AttributeBuilder(): AttributeBuilder = mutable.ListBuffer.empty

  type ChildrenBuilder = mutable.ListBuffer[(String, XmlEntry)]
  def ChildrenBuilder(): ChildrenBuilder = mutable.ListBuffer.empty

  sealed trait Accumulator {
    def toEntry: XmlEntry
  }
  object Accumulator {
    case class TextNode(var text: Option[XmlLeaf] = None) extends Accumulator {

      override def toEntry: XmlLeaf = text.getOrElse {
        throw DecodingError("TextNode.text was empty while traversing xml", Nil)
      }
    }

    case class ParentNode(attributes: AttributeBuilder = AttributeBuilder(),
                          children: ChildrenBuilder = ChildrenBuilder())
        extends Accumulator {

      override def toEntry: XmlNode = {
        XmlNode(attributes.toList, children.toList)
      }
    }

//    case class Both(textName: String, textNode: TextNode, parentNode: ParentNode) extends Accumulator {
//      override def toEntry: XmlEntry = XmlNode(
//        Nil,
//        List(textName -> textNode.toEntry) ++ parentNode.toEntry
//      )
//    }

  }
}
