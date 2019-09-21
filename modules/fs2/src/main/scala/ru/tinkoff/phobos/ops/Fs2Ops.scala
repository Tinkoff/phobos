package ru.tinkoff.phobos.ops

import cats.MonadError
import cats.syntax.flatMap._
import javax.xml.stream.XMLStreamConstants
import ru.tinkoff.phobos.decoding.{Cursor, ElementDecoder, XmlDecoder, XmlStreamReader}
import fs2.Stream

private[phobos] trait Fs2Ops {
  implicit class DecoderOps[A](xmlDecoder: XmlDecoder[A]) {
    def decodeFromStream[F[_], G[_]](stream: Stream[F, Array[Byte]], charset: String = "UTF-8")(
        implicit compiler: Stream.Compiler[F, G],
        monadError: MonadError[G, Throwable]): G[A] = {
      val sr: XmlStreamReader = XmlDecoder.createStreamReader(charset)
      val cursor              = new Cursor(sr)

      stream
        .fold[ElementDecoder[A]](xmlDecoder.elementdecoder) { (decoder, bytes) =>
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
        .map(_.result(cursor.history))
        .compile
        .lastOrError
        .flatMap(result => MonadError[G, Throwable].fromEither(result))
    }
  }
}
