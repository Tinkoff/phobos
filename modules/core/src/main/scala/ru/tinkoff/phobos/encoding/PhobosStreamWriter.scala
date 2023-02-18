package ru.tinkoff.phobos.encoding

import java.math.BigInteger
import javax.xml.namespace.{NamespaceContext, QName}
import javax.xml.stream.XMLStreamException
import org.codehaus.stax2.typed.Base64Variant
import org.codehaus.stax2.{XMLStreamLocation2, XMLStreamReader2, XMLStreamWriter2}
import org.codehaus.stax2.validation.{ValidationProblemHandler, XMLValidationSchema, XMLValidator}
import PhobosStreamWriter.{isValidXmlCharacter, prefixBase}

/** [[PhobosStreamWriter]] implements most methods of [[XMLStreamWriter2]], but it does not extends [[XMLStreamWriter2]]
  *
  * Unlike [[XMLStreamWriter2]], [[PhobosStreamWriter]] does not throw any exceptions if it is used to write invalid
  * characters. Such characters are simply filtered out. However, [[PhobosStreamWriter]] can still throw exceptions if
  * it is used to write invalid attribute or element names or namespaces with invalid characters.
  */
final class PhobosStreamWriter(sw: XMLStreamWriter2) {

  private var discriminatorLocalName: Option[String] = None
  private var discriminatorNamespace: Option[String] = None
  private var discriminatorValue: Option[String]     = None

  /** Writes type-discriminator attribute inside next start element
    *
    * Following code <code> sw.memoizeDiscriminator(Some("http://www.w3.org/2001/XMLSchema-instance"), "type", "dog")
    * sw.writeStartElement("GoodBoy") </code> will result to something like <code> <GoodBoy
    * xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="dog"> </code>
    *
    * This API extension is required to keep ElementEncoder API simple. This method overrides old discriminator if it
    * was already memorized
    *
    * @param namespaceUri
    *   namespace uri of type discriminator
    * @param localName
    *   local name of type discriminator
    * @param value
    *   value of type discriminator
    */
  def memorizeDiscriminator(namespaceUri: Option[String], localName: String, value: String): Unit = {
    discriminatorNamespace = namespaceUri
    discriminatorLocalName = Some(localName)
    discriminatorValue = Some(value)
  }

  def isPropertySupported(name: String): Boolean =
    sw.isPropertySupported(name)

  def setProperty(name: String, value: Any): Boolean =
    sw.setProperty(name, value)

  def getLocation: XMLStreamLocation2 =
    sw.getLocation

  def getEncoding: String =
    sw.getEncoding

  def writeDTD(rootName: String, systemId: String, publicId: String, internalSubset: String): Unit =
    sw.writeDTD(rootName, systemId, publicId, internalSubset)

  def writeFullEndElement(): Unit =
    sw.writeFullEndElement()

  def writeStartDocument(version: String, encoding: String, standAlone: Boolean): Unit =
    sw.writeStartDocument(version, encoding, standAlone)

  def writeSpace(text: String): Unit =
    sw.writeSpace(text.filter(_.isWhitespace))

  def writeRaw(text: String): Unit =
    sw.writeRaw(filterXmlText(text))

  def copyEventFromReader(r: XMLStreamReader2, preserveEventData: Boolean): Unit =
    sw.copyEventFromReader(r, preserveEventData)

  def closeCompletely(): Unit =
    sw.closeCompletely()

  def writeBoolean(value: Boolean): Unit =
    sw.writeBoolean(value)

  def writeInt(value: Int): Unit =
    sw.writeInt(value)

  def writeLong(value: Long): Unit =
    sw.writeLong(value)

  def writeFloat(value: Float): Unit =
    sw.writeFloat(value)

  def writeDouble(value: Double): Unit =
    sw.writeDouble(value)

  def writeInteger(value: BigInteger): Unit =
    sw.writeInteger(value)

  def writeDecimal(value: java.math.BigDecimal): Unit =
    sw.writeDecimal(value)

  def writeQName(value: QName): Unit =
    sw.writeQName(value)

  def writeBinary(value: Array[Byte], from: Int, length: Int): Unit =
    sw.writeBinary(value, from, length)

  def writeBinary(variant: Base64Variant, value: Array[Byte], from: Int, length: Int): Unit =
    sw.writeBinary(variant, value, from, length)

  def writeIntArray(value: Array[Int], from: Int, length: Int): Unit =
    sw.writeIntArray(value, from, length)

  def writeLongArray(value: Array[Long], from: Int, length: Int): Unit =
    sw.writeLongArray(value, from, length)

  def writeFloatArray(value: Array[Float], from: Int, length: Int): Unit =
    sw.writeFloatArray(value, from, length)

  def writeDoubleArray(value: Array[Double], from: Int, length: Int): Unit =
    sw.writeDoubleArray(value, from, length)

  def writeBooleanAttribute(prefix: String, namespaceURI: String, localName: String, value: Boolean): Unit =
    sw.writeBooleanAttribute(prefix, namespaceURI, localName, value)

  def writeIntAttribute(prefix: String, namespaceURI: String, localName: String, value: Int): Unit =
    sw.writeIntAttribute(prefix, namespaceURI, localName, value)

  def writeLongAttribute(prefix: String, namespaceURI: String, localName: String, value: Long): Unit =
    sw.writeLongAttribute(prefix, namespaceURI, localName, value)

  def writeFloatAttribute(prefix: String, namespaceURI: String, localName: String, value: Float): Unit =
    sw.writeFloatAttribute(prefix, namespaceURI, localName, value)

  def writeDoubleAttribute(prefix: String, namespaceURI: String, localName: String, value: Double): Unit =
    sw.writeDoubleAttribute(prefix, namespaceURI, localName, value)

  def writeIntegerAttribute(prefix: String, namespaceURI: String, localName: String, value: BigInteger): Unit =
    sw.writeIntegerAttribute(prefix, namespaceURI, localName, value)

  def writeDecimalAttribute(
      prefix: String,
      namespaceURI: String,
      localName: String,
      value: java.math.BigDecimal,
  ): Unit =
    sw.writeDecimalAttribute(prefix, namespaceURI, localName, value)

  def writeQNameAttribute(prefix: String, namespaceURI: String, localName: String, value: QName): Unit =
    sw.writeQNameAttribute(prefix, namespaceURI, localName, value)

  def writeBinaryAttribute(prefix: String, namespaceURI: String, localName: String, value: Array[Byte]): Unit =
    sw.writeBinaryAttribute(prefix, namespaceURI, localName, value)

  def writeBinaryAttribute(
      variant: Base64Variant,
      prefix: String,
      namespaceURI: String,
      localName: String,
      value: Array[Byte],
  ): Unit =
    sw.writeBinaryAttribute(variant, prefix, namespaceURI, localName, value)

  def writeIntArrayAttribute(prefix: String, namespaceURI: String, localName: String, value: Array[Int]): Unit =
    sw.writeIntArrayAttribute(prefix, namespaceURI, localName, value)

  def writeLongArrayAttribute(prefix: String, namespaceURI: String, localName: String, value: Array[Long]): Unit =
    sw.writeLongArrayAttribute(prefix, namespaceURI, localName, value)

  def writeFloatArrayAttribute(prefix: String, namespaceURI: String, localName: String, value: Array[Float]): Unit =
    sw.writeFloatArrayAttribute(prefix, namespaceURI, localName, value)

  def writeDoubleArrayAttribute(prefix: String, namespaceURI: String, localName: String, value: Array[Double]): Unit =
    sw.writeDoubleArrayAttribute(prefix, namespaceURI, localName, value)

  private def maybeWriteDiscriminator(): Unit = {
    (discriminatorNamespace, discriminatorLocalName, discriminatorValue) match {
      case (None, None, None)                                 =>
      case (None, Some(dLocalName), Some(dValue))             => sw.writeAttribute(dLocalName, dValue)
      case (Some(dNamespace), Some(dLocalName), Some(dValue)) => sw.writeAttribute(dNamespace, dLocalName, dValue)
      case state                                              => throw new XMLStreamException(s"Unexpected discriminator names state: $state")
    }
    discriminatorNamespace = None
    discriminatorLocalName = None
    discriminatorValue = None
  }

  def writeStartElement(localName: String): Unit = {
    sw.writeStartElement(localName: String)
    maybeWriteDiscriminator()
  }

  def writeStartElement(namespaceURI: String, localName: String): Unit = {
    sw.writeStartElement(namespaceURI, localName)
    maybeWriteDiscriminator()
  }

  def writeStartElement(prefix: String, localName: String, namespaceURI: String): Unit = {
    sw.writeStartElement(prefix, localName, namespaceURI)
    maybeWriteDiscriminator()
  }

  def writeEmptyElement(namespaceURI: String, localName: String): Unit =
    sw.writeEmptyElement(namespaceURI, localName)

  def writeEmptyElement(prefix: String, localName: String, namespaceURI: String): Unit =
    sw.writeEmptyElement(prefix, localName, namespaceURI)

  def writeEmptyElement(localName: String): Unit =
    sw.writeEmptyElement(localName)

  def writeEndElement(): Unit =
    sw.writeEndElement()

  def writeEndDocument(): Unit =
    sw.writeEndDocument()

  def close(): Unit =
    sw.close()

  def flush(): Unit =
    sw.flush()

  def writeAttribute(localName: String, value: String): Unit =
    sw.writeAttribute(localName, filterXmlText(value))

  def writeAttribute(prefix: String, namespaceURI: String, localName: String, value: String): Unit =
    sw.writeAttribute(prefix, namespaceURI, localName, filterXmlText(value))

  def writeAttribute(namespaceURI: String, localName: String, value: String): Unit =
    sw.writeAttribute(namespaceURI, localName, filterXmlText(value))

  def writeNamespace(prefix: String, namespaceURI: String): Unit =
    sw.writeNamespace(prefix, namespaceURI)

  /** Writes a namespace to the output stream Generates unbound prefix in format <code>ans\d+</code> and binds it to URI
    *
    * @param namespaceURI
    *   the uri to bind the prefix to
    */
  def writeNamespace(namespaceURI: String): Unit = {
    val nsContext = sw.getNamespaceContext
    if (nsContext.getPrefix(namespaceURI) == null) {
      var left  = 1
      var right = 1
      while (nsContext.getNamespaceURI(prefixBase + right) != null) {
        left = right
        right *= 2
      }
      while (left + 1 < right) {
        val median = (left + right) / 2
        if (nsContext.getNamespaceURI(prefixBase + median) == null) right = median
        else left = median
      }
      sw.writeNamespace(prefixBase + right, namespaceURI)
    }
  }

  def writeDefaultNamespace(namespaceURI: String): Unit =
    sw.writeDefaultNamespace(namespaceURI)

  def writeComment(data: String): Unit =
    sw.writeComment(filterXmlText(data))

  def writeProcessingInstruction(target: String): Unit =
    sw.writeProcessingInstruction(target)

  def writeProcessingInstruction(target: String, data: String): Unit =
    sw.writeProcessingInstruction(target, data)

  def writeCData(data: String): Unit =
    sw.writeCData(filterXmlText(data))

  def writeDTD(dtd: String): Unit =
    sw.writeDTD(dtd)

  def writeEntityRef(name: String): Unit =
    sw.writeEntityRef(name)

  def writeStartDocument(): Unit =
    sw.writeStartDocument()

  def writeStartDocument(version: String): Unit =
    sw.writeStartDocument(version)

  def writeStartDocument(encoding: String, version: String): Unit =
    sw.writeStartDocument(encoding, version)

  def writeCharacters(text: String): Unit =
    sw.writeCharacters(filterXmlText(text))

  def getPrefix(uri: String): String =
    sw.getPrefix(uri)

  def setPrefix(prefix: String, uri: String): Unit =
    sw.setPrefix(prefix, uri)

  def setDefaultNamespace(uri: String): Unit =
    sw.setDefaultNamespace(uri)

  def setNamespaceContext(context: NamespaceContext): Unit =
    sw.setNamespaceContext(context)

  def getNamespaceContext: NamespaceContext =
    sw.getNamespaceContext

  def getProperty(name: String): AnyRef =
    sw.getProperty(name)

  def validateAgainst(schema: XMLValidationSchema): XMLValidator =
    sw.validateAgainst(schema)

  def stopValidatingAgainst(schema: XMLValidationSchema): XMLValidator =
    sw.stopValidatingAgainst(schema)

  def stopValidatingAgainst(validator: XMLValidator): XMLValidator =
    sw.stopValidatingAgainst(validator)

  def setValidationProblemHandler(h: ValidationProblemHandler): ValidationProblemHandler =
    sw.setValidationProblemHandler(h)

  private def filterXmlText(string: String) =
    if (string.forall(PhobosStreamWriter.isValidXmlCharacter)) string
    else string.filter(PhobosStreamWriter.isValidXmlCharacter)
}

object PhobosStreamWriter {
  val prefixBase = "ans"

  def isValidXmlCharacter(char: Char): Boolean =
    !(char < ' '
      || 0xd800 <= char && char <= 0xdfff
      || char == 0xfffe || char == 0xffff)
}
