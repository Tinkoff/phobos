package ru.tinkoff.phobos.decoding

import cats.Foldable
import javax.xml.stream.XMLStreamConstants
import cats.syntax.option._
import com.fasterxml.aalto.{AsyncByteArrayFeeder, WFCException}
import com.fasterxml.aalto.async.{AsyncByteArrayScanner, AsyncStreamReaderImpl}
import com.fasterxml.aalto.stax.InputFactoryImpl
import ru.tinkoff.phobos.Namespace
import ru.tinkoff.phobos.decoding.XmlDecoder.createStreamReader

/**
  * Typeclass for decoding XML document to an A value.
  *
  * XmlDecoder instance must exist only for types which are decoded as XML documents (only for root elements).
  *
  * XmlDecoder instance can be created
  *  - from ElementDecoder using functions in XmlDecoder object
  *  - by macros from ru.tinkoff.phobos.derivation.semiauto package
  *
  * This typeclass wraps ElementDecoder[A] and provides element name and Cursor.
  */
trait XmlDecoder[A] {
  val localname: String
  val namespaceuri: Option[String]
  val elementdecoder: ElementDecoder[A]

  def decode(string: String, charset: String = "UTF-8"): Either[DecodingError, A] =
    decodeFromBytes(string.getBytes(charset), charset)

  def decodeFromBytes(bytes: Array[Byte], charset: String = "UTF-8"): Either[DecodingError, A] = {
    val sr: XmlStreamReader = createStreamReader(charset)

    sr.getInputFeeder.feedInput(bytes, 0, bytes.length)
    sr.getInputFeeder.endOfInput()
    val cursor = new Cursor(sr)
    try {
      do {
        cursor.next()
      } while (cursor.getEventType == XMLStreamConstants.DTD || cursor.getEventType == XMLStreamConstants.START_DOCUMENT)
      elementdecoder
        .decodeAsElement(cursor, localname, namespaceuri)
        .result(cursor.history)
    } catch {
      case e: WFCException =>
        Left(DecodingError(e.getMessage, cursor.history))
    }
  }

  def decodeFromFoldable[F[_]: Foldable](f: F[Array[Byte]], charset: String = "UTF-8"): Either[DecodingError, A] = {
    val sr: XmlStreamReader = createStreamReader(charset)
    val cursor              = new Cursor(sr)

    val a = Foldable[F].foldLeft(f, elementdecoder) { (decoder: ElementDecoder[A], bytes: Array[Byte]) =>
      sr.getInputFeeder.feedInput(bytes, 0, bytes.length)
      do {
        cursor.next()
      } while (cursor.getEventType == XMLStreamConstants.DTD || cursor.getEventType == XMLStreamConstants.START_DOCUMENT)

      if (decoder.result(cursor.history).isRight) {
        decoder
      } else {
        decoder.decodeAsElement(cursor, localname, namespaceuri)
      }
    }
    sr.getInputFeeder.endOfInput()
    a.result(cursor.history)
  }
}

object XmlDecoder {

  def createStreamReader(charset: String): XmlStreamReader = {
    val inputFactory = new InputFactoryImpl
    val cfg          = inputFactory.getNonSharedConfig(null, null, null, false, false)
    cfg.setActualEncoding(charset)
    cfg.doReportCData(false)
    new AsyncStreamReaderImpl[AsyncByteArrayFeeder](new AsyncByteArrayScanner(cfg))
  }

  def apply[A](implicit instance: XmlDecoder[A]): XmlDecoder[A] = instance

  def fromElementDecoder[A](localName: String, namespaceUri: Option[String])(
      implicit elementDecoder: ElementDecoder[A]): XmlDecoder[A] =
    new XmlDecoder[A] {
      val localname: String                 = localName
      val namespaceuri: Option[String]      = namespaceUri
      val elementdecoder: ElementDecoder[A] = elementDecoder
    }

  def fromElementDecoder[A](localName: String)(implicit elementDecoder: ElementDecoder[A]): XmlDecoder[A] =
    fromElementDecoder(localName, None)

  def fromElementDecoderNs[A, NS](localName: String, namespaceInstance: NS)(implicit elementDecoder: ElementDecoder[A],
                                                                            namespace: Namespace[NS]): XmlDecoder[A] =
    fromElementDecoder(localName, namespace.getNamespace.some)

  def fromElementDecoderNs[A, NS](localName: String)(implicit elementDecoder: ElementDecoder[A],
                                                     namespace: Namespace[NS]): XmlDecoder[A] =
    fromElementDecoder(localName, namespace.getNamespace.some)
}
