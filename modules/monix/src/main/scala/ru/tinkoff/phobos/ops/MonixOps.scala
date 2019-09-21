package ru.tinkoff.phobos.ops

import javax.xml.stream.XMLStreamConstants
import monix.eval.Task
import monix.reactive.Observable
import ru.tinkoff.phobos.decoding.{Cursor, ElementDecoder, XmlDecoder, XmlStreamReader}

private[phobos] trait MonixOps {
  implicit class DecoderOps[A](xmlDecoder: XmlDecoder[A]) {
    def decodeFromObservable(observable: Observable[Array[Byte]], charset: String = "UTF-8"): Task[A] = {
      val sr: XmlStreamReader = XmlDecoder.createStreamReader(charset)
      val cursor              = new Cursor(sr)

      observable.foldLeftL[ElementDecoder[A]](xmlDecoder.elementdecoder) { (decoder, bytes) =>
          sr.getInputFeeder.feedInput(bytes, 0, bytes.length)
          do {
            cursor.next()
          } while (cursor.getEventType == XMLStreamConstants.DTD || cursor.getEventType == XMLStreamConstants.START_DOCUMENT)

          if (decoder.result(cursor.history).isRight) {
            decoder
          } else {
            decoder.decodeAsElement(cursor, xmlDecoder.localname, xmlDecoder.namespaceuri)
          }
        }
        .flatMap { a =>
          sr.getInputFeeder.endOfInput()
          Task.fromEither(a.result(cursor.history))
        }
    }
  }
}
