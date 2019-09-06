package ru.tinkoff.phobos.soap

import akka.http.scaladsl.marshalling.{Marshaller, ToEntityMarshaller}
import akka.http.scaladsl.model.HttpEntity
import akka.http.scaladsl.model.ContentTypes.`text/xml(UTF-8)`
import akka.http.scaladsl.unmarshalling.{FromResponseUnmarshaller, Unmarshaller}
import ru.tinkoff.phobos.decoding.XmlDecoder
import ru.tinkoff.phobos.encoding.XmlEncoder

object marshalling {
  implicit def soapMarshaller[T](implicit encoder: XmlEncoder[T]): ToEntityMarshaller[T] =
    Marshaller.withFixedContentType(`text/xml(UTF-8)`) { body =>
      HttpEntity(`text/xml(UTF-8)`, encoder.encode(body))
    }

  implicit def soapUnmarshaller[T](implicit decoder: XmlDecoder[T]): FromResponseUnmarshaller[T] =
    Unmarshaller.messageUnmarshallerFromEntityUnmarshaller(Unmarshaller.stringUnmarshaller.map { str =>
      decoder.decode(str)
    })
}
