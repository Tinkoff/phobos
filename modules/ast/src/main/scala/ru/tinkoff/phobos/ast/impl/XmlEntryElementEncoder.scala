package ru.tinkoff.phobos.ast.impl

import ru.tinkoff.phobos.ast.XmlEntry
import ru.tinkoff.phobos.encoding.{ElementEncoder, PhobosStreamWriter, TextEncoder}

object XmlEntryElementEncoder extends ElementEncoder[XmlEntry] {

  override def encodeAsElement(
      entry: XmlEntry,
      sw: PhobosStreamWriter,
      localName: String,
      namespaceUri: Option[String]
  ): Unit = {
    entry match {
      case leaf: XmlEntry.Leaf =>
        leaf.companion.elementEncoder.encodeAsElement(leaf.value, sw, localName, namespaceUri)

      case node: XmlEntry.Node =>
        namespaceUri.fold(sw.writeStartElement(localName))(sw.writeStartElement(_, localName))
        node.attributes.foreach {
          case (attrName, attrValue) =>
            attrValue.companion.attributeEncoder.encodeAsAttribute(attrValue.value, sw, attrName, namespaceUri = None)
        }
        node match {
          case XmlEntry.TextNode(_, text) =>
            text.companion.textEncoder.encodeAsText(text.value, sw)
          case XmlEntry.ParentNode(_, children) =>
            children foreach {
              case (childName, child) =>
                encodeAsElement(child, sw, childName, namespaceUri = None)
            }
        }
        sw.writeEndElement()
    }
  }
}
