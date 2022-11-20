package ru.tinkoff.phobos.ast

import ru.tinkoff.phobos.encoding.{ElementEncoder, PhobosStreamWriter}

private[phobos] object XmlEntryElementEncoder extends ElementEncoder[XmlEntry] {

  override def encodeAsElement(entry: XmlEntry, sw: PhobosStreamWriter, localName: String, namespaceUri: Option[String], preferredNamespacePrefix: Option[String]): Unit = {
    entry match {
      case leaf: XmlLeaf =>
        leaf.companion.elementEncoder.encodeAsElement(leaf.value, sw, localName, namespaceUri, preferredNamespacePrefix)

      case XmlNode(attributes, children) =>
        namespaceUri.fold(sw.writeStartElement(localName))(sw.writeStartElement(_, localName))
        attributes.foreach { case (attrName, attrValue) =>
          attrValue.companion.attributeEncoder.encodeAsAttribute(attrValue.value, sw, attrName, namespaceUri = None)
        }
        children foreach { case (childName, child) =>
          encodeAsElement(child, sw, childName, namespaceUri = None, preferredNamespacePrefix = None)
        }
        sw.writeEndElement()
    }
  }
}
