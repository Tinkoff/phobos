package ru.tinkoff.phobos.decoding

import cats.Functor
import ru.tinkoff.phobos.decoding.TextDecoder.{EMappedDecoder, MappedDecoder}

trait TextDecoder[A] { self =>
  def decodeAsText(c: Cursor, localName: String, namespaceUri: Option[String]): TextDecoder[A]
  def result(history: List[String]): Either[DecodingError, A]
  def isCompleted: Boolean

  def map[B](f: A => B): TextDecoder[B] = new MappedDecoder(self, f)

  def emap[B](f: (List[String], A) => Either[DecodingError, B]): TextDecoder[B] = new EMappedDecoder(self, f)
}

object TextDecoder extends TextDecoderInstances {

  class MappedDecoder[A, B](fa: TextDecoder[A], f: A => B) extends TextDecoder[B] {

    def decodeAsText(c: Cursor, localName: String, namespaceUri: Option[String]): TextDecoder[B] =
      new MappedDecoder[A, B](fa.decodeAsText(c, localName, namespaceUri), f)

    def result(history: List[String]): Either[DecodingError, B] = fa.result(history).map(f)

    val isCompleted: Boolean = fa.isCompleted

    override def toString: String = s"MappedDecoder(${fa.toString})"
  }

  final class EMappedDecoder[A, B](fa: TextDecoder[A], f: (List[String], A) => Either[DecodingError, B])
      extends TextDecoder[B] {

    def decodeAsText(c: Cursor, localName: String, namespaceUri: Option[String]): TextDecoder[B] =
      new EMappedDecoder(fa.decodeAsText(c, localName, namespaceUri), f)

    def result(history: List[String]): Either[DecodingError, B] = fa.result(history) match {
      case Right(a)    => f(history, a)
      case Left(error) => Left(error)
    }

    def isCompleted: Boolean = fa.isCompleted
  }

  implicit val decoderFunctor: Functor[TextDecoder] =
    new Functor[TextDecoder] {
      def map[A, B](fa: TextDecoder[A])(f: A => B): TextDecoder[B] = fa.map(f)
    }

  final class ConstDecoder[A](a: A) extends TextDecoder[A] {
    def decodeAsText(c: Cursor, localName: String, namespaceUri: Option[String]): TextDecoder[A] = this

    def result(history: List[String]): Either[DecodingError, A] = Right(a)

    val isCompleted: Boolean = true

    override def toString: String = s"ConstDecoder($a)"
  }

  final class FailedDecoder[A](decodingError: DecodingError) extends TextDecoder[A] {
    def decodeAsText(c: Cursor, localName: String, namespaceUri: Option[String]): TextDecoder[A] = this

    def result(history: List[String]): Either[DecodingError, A] = Left(decodingError)

    val isCompleted: Boolean = true

    override def toString: String = s"FailedDecoder($decodingError)"
  }
}
