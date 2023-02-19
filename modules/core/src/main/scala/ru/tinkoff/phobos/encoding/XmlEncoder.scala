package ru.tinkoff.phobos.encoding

import java.io.{ByteArrayOutputStream, StringReader, StringWriter}
import javax.xml.transform._
import javax.xml.transform.stream._
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

  // Methods below previously returned a pure value, now these methods return Either to handle encoding errors.
  // Use -Unsafe versions for the old behavior.
  //
  // Encoding may fail if resulting XML document has illegal characters (https://www.w3.org/TR/xml11/#charsets)
  // or if attribute and element names are invalid, for example "1stElement".

  def encode(a: A, charset: String = "UTF-8"): Either[EncodingError, String] =
    wrapXmlException(encodeUnsafe(a, charset))

  def encodeToBytes(a: A, charset: String = "UTF-8"): Either[EncodingError, Array[Byte]] =
    wrapXmlException(encodeToBytesUnsafe(a, charset))

  def encodeWithConfig(a: A, config: XmlEncoderConfig): Either[EncodingError, String] =
    wrapXmlException(encodeWithConfigUnsafe(a, config))

  def encodeToBytesWithConfig(a: A, config: XmlEncoderConfig): Either[EncodingError, Array[Byte]] =
    wrapXmlException(encodeToBytesWithConfigUnsafe(a, config))

  def encodePrettyWithConfig(a: A, config: XmlEncoderConfig, ident: Int = 2): Either[EncodingError, String] =
    wrapXmlException(encodePrettyWithConfigUnsafe(a, config, ident))

  def encodePretty(a: A, charset: String = "UTF-8", ident: Int = 2): Either[EncodingError, String] =
    wrapXmlException(encodePrettyUnsafe(a, charset, ident))

  // Methods below can throw exceptions in some rare cases. Use "safe" methods returning Either,
  // if you are not sure about the correctness of the data being encoded.
  //
  // These method may throw if resulting XML document has illegal characters ([[https://www.w3.org/TR/xml11/#charsets]])
  // or if attribute and element names are invalid, for example "1stElement".

  def encodeUnsafe(a: A, charset: String = "UTF-8"): String =
    new String(encodeToBytesUnsafe(a, charset), charset)

  def encodeToBytesUnsafe(a: A, charset: String = "UTF-8"): Array[Byte] = {
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

  def encodeWithConfigUnsafe(a: A, config: XmlEncoderConfig): String =
    new String(encodeToBytesWithConfigUnsafe(a, config), config.encoding)

  def encodeToBytesWithConfigUnsafe(a: A, config: XmlEncoderConfig): Array[Byte] = {
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

  /** Warning: Use .encodePrettyWithConfig only for debugging, as it is less performant. For production use
    * .encodeWithConfig
    */
  def encodePrettyWithConfigUnsafe(a: A, config: XmlEncoderConfig, ident: Int = 2): String =
    beautifyXml(encodeWithConfigUnsafe(a, config), ident)

  /** Warning: Use .encodePretty only for debugging, as it is less performant. For production use .encode
    */
  def encodePrettyUnsafe(a: A, charset: String = "UTF-8", ident: Int = 2): String =
    beautifyXml(encodeUnsafe(a, charset), ident)

  private def wrapXmlException[B](xml: => B): Either[EncodingError, B] =
    try { Right(xml) }
    catch { case e: Throwable => Left(EncodingError(Option(e.getMessage).getOrElse("No message provided"), Some(e))) }

  private def beautifyXml(xml: String, ident: Int): String = {
    val t = XmlEncoder.transformerFactory.newTransformer()
    t.setOutputProperty(OutputKeys.INDENT, "yes")
    t.setOutputProperty("{http://xml.apache.org/xslt}indent-amount", ident.toString)
    val out = new StringWriter()
    t.transform(new StreamSource(new StringReader(xml)), new StreamResult(out))
    out.toString
  }
}

object XmlEncoder {

  private lazy val factory = {
    val factory = new OutputFactoryImpl
    factory.setProperty("javax.xml.stream.isRepairingNamespaces", true)
    factory
  }

  private lazy val transformerFactory =
    TransformerFactory.newInstance()

  def apply[A](implicit instance: XmlEncoder[A]): XmlEncoder[A] = instance

  def fromElementEncoder[A](localName: String, namespaceUri: Option[String], preferredNamespacePrefix: Option[String])(
      implicit elementEncoder: ElementEncoder[A],
  ): XmlEncoder[A] = {
    val _localName                = localName
    val _namespaceUri             = namespaceUri
    val _elementEncoder           = elementEncoder
    val _preferredNamespacePrefix = preferredNamespacePrefix
    new XmlEncoder[A] {
      val localName: String                                 = _localName
      val namespaceUri: Option[String]                      = _namespaceUri
      val elementEncoder: ElementEncoder[A]                 = _elementEncoder
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
