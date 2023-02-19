package ru.tinkoff.phobos.akka_http.marshalling

import akka.http.scaladsl.marshalling.{Marshaller, ToEntityMarshaller}
import akka.http.scaladsl.model.{HttpCharsets, HttpEntity, MediaTypes}
import akka.http.scaladsl.unmarshalling.{FromEntityUnmarshaller, Unmarshaller}
import ru.tinkoff.phobos.decoding.XmlDecoder
import ru.tinkoff.phobos.encoding.XmlEncoder

object application {
  implicit def soapApplicationXmlMarshaller[T](implicit encoder: XmlEncoder[T]): ToEntityMarshaller[T] =
    Marshaller.withFixedContentType(MediaTypes.`application/xml` withCharset HttpCharsets.`UTF-8`) { body =>
      HttpEntity(MediaTypes.`application/xml` withCharset HttpCharsets.`UTF-8`, encoder.encodeUnsafe(body))
    }

  implicit def soapApplicationXmlUnmarshaller[T](implicit decoder: XmlDecoder[T]): FromEntityUnmarshaller[T] =
    Unmarshaller.stringUnmarshaller.forContentTypes(MediaTypes.`application/xml` withCharset HttpCharsets.`UTF-8`).map {
      body => decoder.decode(body).fold(err => throw err, identity)
    }
}
