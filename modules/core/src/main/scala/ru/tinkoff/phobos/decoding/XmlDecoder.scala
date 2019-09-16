package ru.tinkoff.phobos.decoding

import javax.xml.stream.XMLStreamConstants
import monix.eval.Task
import monix.reactive.Observable
import cats.syntax.option._
import com.fasterxml.aalto.AsyncByteArrayFeeder
import com.fasterxml.aalto.async.{AsyncByteArrayScanner, AsyncStreamReaderImpl}
import com.fasterxml.aalto.stax.InputFactoryImpl
import ru.tinkoff.phobos.Namespace

trait XmlDecoder[A] {
  def decodeFromBytes(a: Array[Byte]): A
  def decodeFromBytesObservable(fa: Observable[Array[Byte]]): Task[A]

  def decode(a: String): A =
    decodeFromBytes(a.getBytes("UTF-8"))

  def decodeFromObservable(fa: Observable[String]): Task[A] =
    decodeFromBytesObservable(fa.map(_.getBytes("UTF-8")))
}

object XmlDecoder {

  def createStreamReader: XmlStreamReader = {
    val inputFactory = new InputFactoryImpl
    val cfg          = inputFactory.getNonSharedConfig(null, null, null, false, false)
    cfg.setActualEncoding("UTF-8")
    cfg.doReportCData(false)
    new AsyncStreamReaderImpl[AsyncByteArrayFeeder](new AsyncByteArrayScanner(cfg))
  }

  def apply[A](implicit instance: XmlDecoder[A]): XmlDecoder[A] = instance

  def fromElementDecoder[A](localName: String, namespaceUri: Option[String])(
      implicit elementDecoder: ElementDecoder[A]): XmlDecoder[A] =
    new XmlDecoder[A] {
      def decodeFromBytesObservable(fa: Observable[Array[Byte]]): Task[A] = {
        val sr: XmlStreamReader = createStreamReader
        val cursor              = new Cursor(sr)

        fa.foldLeftL[ElementDecoder[A]](elementDecoder) { (decoder, bytes) =>
            sr.getInputFeeder.feedInput(bytes, 0, bytes.length)
            do {
              cursor.next()
            } while (cursor.getEventType == XMLStreamConstants.DTD || cursor.getEventType == XMLStreamConstants.START_DOCUMENT)

            if (decoder.result(cursor.history).isRight) {
              decoder
            } else {
              decoder.decodeAsElement(cursor, localName, namespaceUri)
            }
          }
          .map { a =>
            sr.getInputFeeder.endOfInput()
            a.result(cursor.history).fold(err => throw err, identity)
          }
      }

      def decodeFromBytes(a: Array[Byte]): A = {
        val sr: XmlStreamReader = createStreamReader

        sr.getInputFeeder.feedInput(a, 0, a.length)
        sr.getInputFeeder.endOfInput()
        val cursor = new Cursor(sr)
        do {
          cursor.next()
        } while (cursor.getEventType == XMLStreamConstants.DTD || cursor.getEventType == XMLStreamConstants.START_DOCUMENT)
        elementDecoder
          .decodeAsElement(cursor, localName, namespaceUri)
          .result(cursor.history)
          .fold(err => throw err, identity)
      }
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
