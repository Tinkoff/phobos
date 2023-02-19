package ru.tinkoff.phobos.akka_http.marshalling

import akka.http.scaladsl.marshalling.{Marshaller, ToEntityMarshaller}
import akka.http.scaladsl.model.ContentTypes.`text/xml(UTF-8)`
import akka.http.scaladsl.model.HttpEntity
import akka.http.scaladsl.unmarshalling.{FromEntityUnmarshaller, Unmarshaller}
import ru.tinkoff.phobos.decoding.XmlDecoder
import ru.tinkoff.phobos.encoding.XmlEncoder

object text {
  implicit def soapTextXmlMarshaller[T](implicit encoder: XmlEncoder[T]): ToEntityMarshaller[T] =
    Marshaller.withFixedContentType(`text/xml(UTF-8)`) { body =>
      HttpEntity(`text/xml(UTF-8)`, encoder.encodeUnsafe(body))
    }

  implicit def soapTextXmlUnmarshaller[T](implicit decoder: XmlDecoder[T]): FromEntityUnmarshaller[T] =
    Unmarshaller.stringUnmarshaller.forContentTypes(`text/xml(UTF-8)`).map { body =>
      decoder.decode(body).fold(err => throw err, identity)
    }
}
