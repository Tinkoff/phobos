package ru.tinkoff.phobos.decoding

import cats.Functor

trait AttributeDecoder[A] { self =>
  def decodeAsAttribute(c: Cursor, localName: String, namespaceUri: Option[String] = None): Either[DecodingError, A]

  def map[B](f: A => B): AttributeDecoder[B] =
    new AttributeDecoder[B] {
      def decodeAsAttribute(c: Cursor,
                            localName: String,
                            namespaceUri: Option[String] = None): Either[DecodingError, B] =
        self.decodeAsAttribute(c, localName, namespaceUri).map(f)
    }

  def emap[B](f: (List[String], A) => Either[DecodingError, B]): AttributeDecoder[B] =
    new AttributeDecoder[B] {
      def decodeAsAttribute(c: Cursor, localName: String, namespaceUri: Option[String]): Either[DecodingError, B] =
        self.decodeAsAttribute(c, localName, namespaceUri) match {
          case Right(a)    => f(c.history, a)
          case Left(error) => Left(error)
        }
    }
}

object AttributeDecoder extends AttributeDecoderInstances {
  implicit val attributeDecoderFunctor: Functor[AttributeDecoder] =
    new Functor[AttributeDecoder] {
      def map[A, B](fa: AttributeDecoder[A])(f: A => B): AttributeDecoder[B] = fa.map(f)
    }
}
