package ru.tinkoff.phobos.decoding

import cats.Functor
import ru.tinkoff.phobos.decoding.ElementDecoder.{EMappedDecoder, MappedDecoder}

trait ElementDecoder[A] { self =>
  def decodeAsElement(c: Cursor, localName: String, namespaceUri: Option[String] = None): ElementDecoder[A]
  def result(history: List[String]): Either[DecodingError, A]
  def isCompleted: Boolean

  def map[B](f: A => B): ElementDecoder[B] = new MappedDecoder(self, f)

  def emap[B](f: (List[String], A) => Either[DecodingError, B]): ElementDecoder[B] = new EMappedDecoder(self, f)
}

object ElementDecoder extends ElementDecoderInstances {

  def errorIfWrongName[A](c: Cursor, localName: String, namespaceUri: Option[String]): Option[FailedDecoder[A]] = {
    namespaceUri match {
      case _ if c.getLocalName != localName =>
        Some(new FailedDecoder(c.error(s"Invalid local name. Expected '$localName', but found '${c.getLocalName}'")))
      case Some(uri) if uri != c.getNamespaceURI =>
        Some(new FailedDecoder(c.error(s"Invalid namespace. Expected '$uri', but found '${c.getNamespaceURI}'")))
      case None if c.getNamespaceURI != "" =>
        Some(new FailedDecoder(c.error(s"Invalid namespace. Expected no namespace, but found '${c.getNamespaceURI}'")))
      case _ => None
    }
  }

  def isNil(c: Cursor): Boolean = {
    val nilIdx = c.getAttributeIndex("http://www.w3.org/2001/XMLSchema-instance", "nil")
    nilIdx > -1 && c.getAttributeValue(nilIdx) == "true"
  }

  final class MappedDecoder[A, B](fa: ElementDecoder[A], f: A => B) extends ElementDecoder[B] {
    def decodeAsElement(c: Cursor, localName: String, namespaceUri: Option[String] = None): ElementDecoder[B] =
      new MappedDecoder(fa.decodeAsElement(c, localName, namespaceUri), f)

    def result(history: List[String]): Either[DecodingError, B] = fa.result(history).map(f)

    val isCompleted: Boolean = fa.isCompleted

    override def toString: String = s"MappedDecoder(${fa.toString})"
  }

  final class EMappedDecoder[A, B](fa: ElementDecoder[A], f: (List[String], A) => Either[DecodingError, B])
      extends ElementDecoder[B] {
    def decodeAsElement(c: Cursor, localName: String, namespaceUri: Option[String]): ElementDecoder[B] =
      new EMappedDecoder(fa.decodeAsElement(c, localName, namespaceUri), f)

    def result(history: List[String]): Either[DecodingError, B] =
      fa.result(history) match {
        case Right(a)    => f(history, a)
        case Left(error) => Left(error)
      }

    def isCompleted: Boolean = fa.isCompleted
  }

  implicit val decoderFunctor: Functor[ElementDecoder] =
    new Functor[ElementDecoder] {
      def map[A, B](fa: ElementDecoder[A])(f: A => B): ElementDecoder[B] = fa.map(f)
    }

  final class ConstDecoder[A](a: A) extends ElementDecoder[A] {
    def decodeAsElement(c: Cursor, localName: String, namespaceUri: Option[String] = None): ElementDecoder[A] =
      new FailedDecoder[A](c.error("Element is already decoded (Most likely it occurred more than once)"))

    def result(history: List[String]): Either[DecodingError, A] = Right(a)

    val isCompleted: Boolean = true

    override def toString: String = s"ConstDecoder($a)"
  }

  final class FailedDecoder[A](decodingError: DecodingError) extends ElementDecoder[A] {
    def decodeAsElement(c: Cursor, localName: String, namespaceUri: Option[String] = None): ElementDecoder[A] = this

    def result(history: List[String]): Either[DecodingError, A] = Left(decodingError)

    val isCompleted: Boolean = true

    override def toString: String = s"FailedDecoder($decodingError)"
  }
}
