package ru.tinkoff.phobos.decoding

import java.util.Base64

import scala.util.{Failure, Success, Try}

trait ValueDecoderInstances {

  implicit val booleanDecoder: ValueDecoder[Boolean] = (history, value) =>
    value match {
      case "true" | "1"  => Right(true)
      case "false" | "0" => Right(false)
      case str           => Left(DecodingError(s"Value `$str` is not `true` or `false`", history))
  }

  implicit val charDecoder: ValueDecoder[Char] = (history, value) => {
    if (value.length != 1) {
      Left(DecodingError("Value too long for char", history))
    } else {
      Right(value.head)
    }
  }

  def wrapException[A](f: String => A): ValueDecoder[A] =
    (history, value) =>
      Try(f(value)) match {
        case Failure(exception) => Left(DecodingError(exception.getMessage, history))
        case Success(a)         => Right(a)
    }

  implicit val byteDecoder: ValueDecoder[Byte]             = wrapException(_.toByte)
  implicit val shortDecoder: ValueDecoder[Short]           = wrapException(_.toShort)
  implicit val intDecoder: ValueDecoder[Int]               = wrapException(_.toInt)
  implicit val longDecoder: ValueDecoder[Long]             = wrapException(_.toLong)
  implicit val floatDecoder: ValueDecoder[Float]           = wrapException(_.toFloat)
  implicit val doubleDecoder: ValueDecoder[Double]         = wrapException(_.toDouble)
  implicit val bigDecimalDecoder: ValueDecoder[BigDecimal] = wrapException(BigDecimal.apply)
  implicit val bigIntDecoder: ValueDecoder[BigInt]         = wrapException(BigInt.apply)
  implicit val base64Decoder: ValueDecoder[Array[Byte]]    = wrapException(Base64.getDecoder.decode _) // TODO: hexDecoder

}
