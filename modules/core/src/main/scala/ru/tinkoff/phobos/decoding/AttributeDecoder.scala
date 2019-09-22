package ru.tinkoff.phobos.decoding

import java.time.{LocalDate, LocalDateTime, LocalTime, ZonedDateTime}
import java.util.{Base64, UUID}

import cats.Functor

trait AttributeDecoder[A] { self =>
  def decodeAsAttribute(c: Cursor, localName: String, namespaceUri: Option[String]): Either[DecodingError, A]

  def map[B](f: A => B): AttributeDecoder[B] =
    new AttributeDecoder[B] {
      def decodeAsAttribute(c: Cursor, localName: String, namespaceUri: Option[String]): Either[DecodingError, B] =
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

object AttributeDecoder {
  implicit val attributeDecoderFunctor: Functor[AttributeDecoder] =
    new Functor[AttributeDecoder] {
      def map[A, B](fa: AttributeDecoder[A])(f: A => B): AttributeDecoder[B] = fa.map(f)
    }

  /**
    * Instances
    */
  implicit val stringDecoder: AttributeDecoder[String] =
    new AttributeDecoder[String] {
      def decodeAsAttribute(c: Cursor,
                            localName: String,
                            namespaceUri: Option[String]): Either[DecodingError, String] = {
        val idx = c.getAttributeIndex(namespaceUri.orNull, localName)
        if (idx > -1) {
          Right(c.getAttributeValue(idx))
        } else {
          Left(c.error(s"Missing '$localName' attribute"))
        }
      }
    }

  implicit val unitDecoder: AttributeDecoder[Unit] = new AttributeDecoder[Unit] {
    def decodeAsAttribute(c: Cursor, localName: String, namespaceUri: Option[String]): Either[DecodingError, Unit] =
      Right(())
  }

  implicit val booleanDecoder: AttributeDecoder[Boolean] =
    stringDecoder.emap((history, string) =>
      string match {
        case "true" | "1"  => Right(true)
        case "false" | "0" => Right(false)
        case str           => Left(DecodingError(s"Value `$str` is not `true` or `false`", history))
    })

  implicit val javaBooleanDecoder: AttributeDecoder[java.lang.Boolean] = booleanDecoder.map(_.booleanValue())

  implicit val charDecoder: AttributeDecoder[Char] =
    stringDecoder.emap((history, string) => {
      if (string.length != 1) {
        Left(DecodingError("Value too long for char", history))
      } else {
        Right(string.head)
      }
    })
  implicit val javaCharacterDecoder: AttributeDecoder[java.lang.Character] = charDecoder.map(_.charValue())
  implicit val floatDecoder: AttributeDecoder[Float]                       = stringDecoder.emap(wrapException(_.toFloat))
  implicit val javaFloatDecoder: AttributeDecoder[java.lang.Float]         = floatDecoder.map(_.floatValue())
  implicit val doubleDecoder: AttributeDecoder[Double]                     = stringDecoder.emap(wrapException(_.toDouble))
  implicit val javaDoubleDecoder: AttributeDecoder[java.lang.Double]       = doubleDecoder.map(_.doubleValue())
  implicit val byteDecoder: AttributeDecoder[Byte]                         = stringDecoder.emap(wrapException(_.toByte))
  implicit val javaByteDecoder: AttributeDecoder[java.lang.Byte]           = byteDecoder.map(_.byteValue())
  implicit val shortDecoder: AttributeDecoder[Short]                       = stringDecoder.emap(wrapException(_.toShort))
  implicit val javaShortDecoder: AttributeDecoder[java.lang.Short]         = shortDecoder.map(_.shortValue())
  implicit val intDecoder: AttributeDecoder[Int]                           = stringDecoder.emap(wrapException(_.toInt))
  implicit val javaIntegerDecoder: AttributeDecoder[java.lang.Integer]     = intDecoder.map(_.intValue())
  implicit val longDecoder: AttributeDecoder[Long]                         = stringDecoder.emap(wrapException(_.toLong))
  implicit val javaLongDecoder: AttributeDecoder[java.lang.Long]           = longDecoder.map(_.longValue())
  implicit val bigIntDecoder: AttributeDecoder[BigInt]                     = stringDecoder.emap(wrapException(BigInt.apply))

  implicit val javaBigIntegerDecoder: AttributeDecoder[java.math.BigInteger] =
    stringDecoder.emap(wrapException(str => new java.math.BigInteger(str)))

  implicit val bigDecimalDecoder: AttributeDecoder[BigDecimal]               = stringDecoder.map(BigDecimal.apply)
  implicit val javaBigDecimalDecoder: AttributeDecoder[java.math.BigDecimal] = bigDecimalDecoder.map(_.bigDecimal)
  implicit val UUIDDecoder: AttributeDecoder[UUID]                           = stringDecoder.emap(wrapException(UUID.fromString))

  implicit val base64Decoder: AttributeDecoder[Array[Byte]] =
    stringDecoder.emap(wrapException(Base64.getDecoder.decode))

  implicit def optionDecoder[A](implicit decoder: AttributeDecoder[A]): AttributeDecoder[Option[A]] =
    new AttributeDecoder[Option[A]] {
      def decodeAsAttribute(c: Cursor,
                            localName: String,
                            namespaceUri: Option[String]): Either[DecodingError, Option[A]] = {
        val idx = c.getAttributeIndex(namespaceUri.orNull, localName)
        if (idx > -1) {
          decoder.decodeAsAttribute(c, localName, namespaceUri).map(Some.apply)
        } else {
          Right(None)
        }
      }
    }

  implicit def someDecoder[A](implicit e: AttributeDecoder[A]): AttributeDecoder[Some[A]] = e.map(Some.apply)

  implicit val noneDecoder: AttributeDecoder[None.type] =
    new AttributeDecoder[None.type] {
      def decodeAsAttribute(c: Cursor,
                            localName: String,
                            namespaceUri: Option[String]): Either[DecodingError, None.type] = Right(None)
    }

  implicit val localDateTimeDecoder: AttributeDecoder[LocalDateTime] =
    stringDecoder.emap(wrapException(LocalDateTime.parse))

  implicit val zonedDateTimeDecoder: AttributeDecoder[ZonedDateTime] =
    stringDecoder.emap(wrapException(ZonedDateTime.parse))

  implicit val localDateDecoder: AttributeDecoder[LocalDate] =
    stringDecoder.emap(wrapException(LocalDate.parse))

  implicit val localTimeDecoder: AttributeDecoder[LocalTime] =
    stringDecoder.emap(wrapException(LocalTime.parse))
}
