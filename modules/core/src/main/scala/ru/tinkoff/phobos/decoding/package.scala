package ru.tinkoff.phobos

import com.fasterxml.aalto.{AsyncByteArrayFeeder, AsyncXMLStreamReader}

import scala.util.{Failure, Success, Try}

package object decoding {
  type XmlStreamReader = AsyncXMLStreamReader[AsyncByteArrayFeeder]

  private[decoding] def wrapException[A](f: String => A) =
    (history: List[String], string: String) =>
      Try(f(string)) match {
        case Failure(exception) => Left(DecodingError(exception.getMessage, history))
        case Success(a)         => Right(a)
      }
}
