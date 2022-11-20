package ru.tinkoff.phobos.encoding

import java.io.ByteArrayOutputStream

import com.fasterxml.aalto.stax.OutputFactoryImpl
import org.codehaus.stax2.XMLStreamWriter2
import ru.tinkoff.phobos.Namespace
import ru.tinkoff.phobos.encoding.XmlEncoder.XmlEncoderConfig

/** Typeclass for encoding XML document to an A value.
  *
  * XmlEncoder instance must exist only for types which are encoded as XML documents (only for root elements).
  *
  * XmlEncoder instance can be created
  *   - from ElementEncoder using functions in XmlEncoder object
  *   - by macros from ru.tinkoff.phobos.derivation.semiauto package
  *
  * This typeclass wraps ElementEncoder[A] and provides element name and StreamWriter.
  */
trait XmlEncoder[A] {
  val localName: String
  val namespaceUri: Option[String]
  val preferredNamespacePrefix: Option[String] = None
  val elementEncoder: ElementEncoder[A]

  def encode(a: A, charset: String = "UTF-8"): String =
    new String(encodeToBytes(a, charset), charset)

  def encodeToBytes(a: A, charset: String = "UTF-8"): Array[Byte] = {
    val os = new ByteArrayOutputStream
    val sw =
      new PhobosStreamWriter(XmlEncoder.factory.createXMLStreamWriter(os, charset).asInstanceOf[XMLStreamWriter2])
    sw.writeStartDocument()
    elementEncoder.encodeAsElement(a, sw, localName, namespaceUri, preferredNamespacePrefix)
    sw.writeEndDocument()
    sw.flush()
    sw.close()
    os.toByteArray
  }

  def encodeWithConfig(a: A, config: XmlEncoderConfig): String =
    new String(encodeToBytesWithConfig(a, config), config.encoding)

  def encodeToBytesWithConfig(a: A, config: XmlEncoderConfig): Array[Byte] = {
    val os = new ByteArrayOutputStream
    val sw =
      new PhobosStreamWriter(
        XmlEncoder.factory.createXMLStreamWriter(os, config.encoding).asInstanceOf[XMLStreamWriter2],
      )
    if (config.writeProlog) {
      sw.writeStartDocument(config.encoding, config.version)
    }
    elementEncoder.encodeAsElement(a, sw, localName, namespaceUri, preferredNamespacePrefix)
    if (config.writeProlog) {
      sw.writeEndDocument()
    }
    sw.flush()
    sw.close()
    os.toByteArray
  }

}

object XmlEncoder {

  private lazy val factory = {
    val factory = new OutputFactoryImpl
    factory.setProperty("javax.xml.stream.isRepairingNamespaces", true)
    factory
  }

  def apply[A](implicit instance: XmlEncoder[A]): XmlEncoder[A] = instance

  def fromElementEncoder[A](localName: String, namespaceUri: Option[String], preferredNamespacePrefix: Option[String])(
      implicit elementEncoder: ElementEncoder[A],
  ): XmlEncoder[A] = {
    val _localName = localName
    val _namespaceUri = namespaceUri
    val _elementEncoder = elementEncoder
    val _preferredNamespacePrefix = preferredNamespacePrefix
    new XmlEncoder[A] {
      val localName: String                 = _localName
      val namespaceUri: Option[String]      = _namespaceUri
      val elementEncoder: ElementEncoder[A] = _elementEncoder
      override val preferredNamespacePrefix: Option[String] = _preferredNamespacePrefix
    }
  }

  def fromElementEncoder[A](localName: String)(implicit elementEncoder: ElementEncoder[A]): XmlEncoder[A] =
    fromElementEncoder(localName, None, None)

  def fromElementEncoderNs[A, NS](localName: String, namespaceInstance: NS)(
      implicit elementEncoder: ElementEncoder[A],
      namespace: Namespace[NS],
  ): XmlEncoder[A] =
    fromElementEncoder(localName, Some(namespace.getNamespace), namespace.getPreferredPrefix)

  def fromElementEncoderNs[A, NS](
      localName: String,
  )(implicit elementEncoder: ElementEncoder[A], namespace: Namespace[NS]): XmlEncoder[A] =
    fromElementEncoder(localName, Some(namespace.getNamespace), namespace.getPreferredPrefix)

  final case class XmlEncoderConfig(
      encoding: String,
      version: String,
      writeProlog: Boolean,
  ) {
    def withoutProlog: XmlEncoderConfig = copy(writeProlog = false)
  }

  val defaultConfig: XmlEncoderConfig =
    XmlEncoderConfig(
      encoding = "UTF-8",
      version = "1.0",
      writeProlog = true,
    )
}
