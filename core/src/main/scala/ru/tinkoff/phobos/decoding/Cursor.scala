package ru.tinkoff.phobos.decoding

import java.io.Writer
import java.math.{BigDecimal, BigInteger}

import javax.xml.namespace.{NamespaceContext, QName}
import javax.xml.stream.{Location, XMLStreamConstants}
import org.codehaus.stax2.typed.{Base64Variant, TypedArrayDecoder, TypedValueDecoder}
import org.codehaus.stax2.{AttributeInfo, LocationInfo}

class Cursor(private val sr: XmlStreamReader) {

  private var historyStack: List[String] = Nil

  def next(): Int = {
    val next = sr.next()
    next match {
      case XMLStreamConstants.START_ELEMENT =>
        historyStack = sr.getLocalName :: historyStack
      case XMLStreamConstants.END_ELEMENT =>
        historyStack = historyStack.tail
      case _ =>
    }
    next
  }

  def hasNext: Boolean = sr.hasNext

  def history: List[String] = historyStack
  def error(text: String): DecodingError = DecodingError(text, historyStack)

  def getAttributeInfo: AttributeInfo = sr.getAttributeInfo
  def getLocationInfo: LocationInfo = sr.getLocationInfo
  def getText(w: Writer, preserveContents: Boolean): Int = sr.getText(w, preserveContents)
  def isEmptyElement: Boolean = sr.isEmptyElement
  def getDepth: Int = sr.getDepth
  def getNonTransientNamespaceContext: NamespaceContext = sr.getNonTransientNamespaceContext
  def getPrefixedName: String = sr.getPrefixedName
  def getElementAsBoolean: Boolean = sr.getElementAsBoolean
  def getElementAsInt: Int = sr.getElementAsInt
  def getElementAsLong: Long = sr.getElementAsLong
  def getElementAsFloat: Float = sr.getElementAsFloat
  def getElementAsDouble: Double = sr.getElementAsDouble
  def getElementAsInteger: BigInteger = sr.getElementAsInteger
  def getElementAsDecimal: BigDecimal = sr.getElementAsDecimal
  def getElementAsQName: QName = sr.getElementAsQName
  def getElementAsBinary: Array[Byte] = sr.getElementAsBinary
  def getElementAsBinary(variant: Base64Variant): Array[Byte] = sr.getElementAsBinary(variant)
  def getElementAs(tvd: TypedValueDecoder): Unit = sr.getElementAs(tvd)

  def readElementAsBinary(resultBuffer: Array[Byte], offset: Int, maxLength: Int, variant: Base64Variant): Int =
    sr.readElementAsBinary(resultBuffer, offset, maxLength, variant)
  def readElementAsBinary(resultBuffer: Array[Byte], offset: Int, maxLength: Int): Int =
    sr.readElementAsBinary(resultBuffer, offset, maxLength)
  def readElementAsIntArray(resultBuffer: Array[Int], offset: Int, length: Int): Int =
    sr.readElementAsIntArray(resultBuffer, offset, length)
  def readElementAsLongArray(resultBuffer: Array[Long], offset: Int, length: Int): Int =
    sr.readElementAsLongArray(resultBuffer, offset, length)
  def readElementAsFloatArray(resultBuffer: Array[Float], offset: Int, length: Int): Int =
    sr.readElementAsFloatArray(resultBuffer, offset, length)
  def readElementAsDoubleArray(resultBuffer: Array[Double], offset: Int, length: Int): Int =
    sr.readElementAsDoubleArray(resultBuffer, offset, length)

  def readElementAsArray(tad: TypedArrayDecoder): Int = sr.readElementAsArray(tad)
  def getAttributeIndex(namespaceURI: String, localName: String): Int = sr.getAttributeIndex(namespaceURI, localName)
  def getAttributeAsBoolean(index: Int): Boolean = sr.getAttributeAsBoolean(index)
  def getAttributeAsInt(index: Int): Int = sr.getAttributeAsInt(index)
  def getAttributeAsLong(index: Int): Long = sr.getAttributeAsLong(index)
  def getAttributeAsFloat(index: Int): Float = sr.getAttributeAsFloat(index)
  def getAttributeAsDouble(index: Int): Double = sr.getAttributeAsDouble(index)
  def getAttributeAsInteger(index: Int): BigInteger = sr.getAttributeAsInteger(index)
  def getAttributeAsDecimal(index: Int): BigDecimal = sr.getAttributeAsDecimal(index)
  def getAttributeAsQName(index: Int): QName = sr.getAttributeAsQName(index)
  def getAttributeAs(index: Int, tvd: TypedValueDecoder): Unit = sr.getAttributeAs(index, tvd)
  def getAttributeAsBinary(index: Int): Array[Byte] = sr.getAttributeAsBinary(index)
  def getAttributeAsBinary(index: Int, v: Base64Variant): Array[Byte] = sr.getAttributeAsBinary(index, v)
  def getAttributeAsIntArray(index: Int): Array[Int] = sr.getAttributeAsIntArray(index)
  def getAttributeAsLongArray(index: Int): Array[Long] = sr.getAttributeAsLongArray(index)
  def getAttributeAsFloatArray(index: Int): Array[Float] = sr.getAttributeAsFloatArray(index)
  def getAttributeAsDoubleArray(index: Int): Array[Double] = sr.getAttributeAsDoubleArray(index)
  def getAttributeAsArray(index: Int, tad: TypedArrayDecoder): Int = sr.getAttributeAsArray(index, tad)

  def getElementText: String = sr.getElementText
//  def nextTag: Int = sr.nextTag

  def getNamespaceURI(prefix: String): String = sr.getNamespaceURI(prefix)
  def isStartElement: Boolean = sr.isStartElement
  def isEndElement: Boolean = sr.isEndElement
  def isCharacters: Boolean = sr.isCharacters
  def isWhiteSpace: Boolean = sr.isWhiteSpace
  def getAttributeValue(namespaceURI: String, localName: String): String = sr.getAttributeValue(namespaceURI, localName)
  def getAttributeCount: Int = sr.getAttributeCount
  def getAttributeName(index: Int): QName = sr.getAttributeName(index)
  def getAttributeNamespace(index: Int): String = sr.getAttributeNamespace(index)
  def getAttributeLocalName(index: Int): String = sr.getAttributeLocalName(index)
  def getAttributePrefix(index: Int): String = sr.getAttributePrefix(index)
  def getAttributeType(index: Int): String = sr.getAttributeType(index)
  def getAttributeValue(index: Int): String = sr.getAttributeValue(index)
  def isAttributeSpecified(index: Int): Boolean = sr.isAttributeSpecified(index)
  def getNamespaceCount: Int = sr.getNamespaceCount
  def getNamespacePrefix(index: Int): String = sr.getNamespacePrefix(index)
  def getNamespaceURI(index: Int): String = sr.getNamespaceURI(index)
  def getNamespaceContext: NamespaceContext = sr.getNamespaceContext
  def getEventType: Int = sr.getEventType
  def getText: String = sr.getText
  def getTextCharacters: Array[Char] = sr.getTextCharacters
  def getTextCharacters(sourceStart: Int, target: Array[Char], targetStart: Int, length: Int): Int =
    sr.getTextCharacters(sourceStart, target, targetStart, length)
  def getTextStart: Int = sr.getTextStart
  def getTextLength: Int = sr.getTextLength
  def getEncoding: String = sr.getEncoding
  def hasText: Boolean = sr.hasText
  def getLocation: Location = sr.getLocation
  def getName: QName = sr.getName
  def getLocalName: String = sr.getLocalName
  def hasName: Boolean = sr.hasName
  def getNamespaceURI: String = sr.getNamespaceURI
  def getPrefix: String = sr.getPrefix
  def getVersion: String = sr.getVersion
  def isStandalone: Boolean = sr.isStandalone
  def standaloneSet: Boolean = sr.standaloneSet
  def getCharacterEncodingScheme: String = sr.getCharacterEncodingScheme
  def getPITarget: String = sr.getPITarget
  def getPIData: String = sr.getPIData
}
