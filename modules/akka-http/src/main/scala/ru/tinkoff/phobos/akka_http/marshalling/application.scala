package ru.tinkoff.phobos.akka_http.marshalling

import akka.http.scaladsl.marshalling.{Marshaller, ToEntityMarshaller}
import akka.http.scaladsl.model.{HttpCharsets, HttpEntity, MediaTypes}
import akka.http.scaladsl.unmarshalling.{FromResponseUnmarshaller, Unmarshaller}
import ru.tinkoff.phobos.decoding.XmlDecoder
import ru.tinkoff.phobos.encoding.XmlEncoder

object application {
  implicit def soapApplicationXmlMarshaller[T](implicit encoder: XmlEncoder[T]): ToEntityMarshaller[T] =
    Marshaller.withFixedContentType(MediaTypes.`application/xml` withCharset HttpCharsets.`UTF-8`) { body =>
      HttpEntity(MediaTypes.`application/xml` withCharset HttpCharsets.`UTF-8`, encoder.encode(body))
    }

  implicit def soapApplicationXmlUnmarshaller[T](implicit decoder: XmlDecoder[T]): FromResponseUnmarshaller[T] =
    Unmarshaller.messageUnmarshallerFromEntityUnmarshaller(Unmarshaller.stringUnmarshaller.map { str =>
      decoder.decode(str).fold(err => throw err, identity)
    })
}
