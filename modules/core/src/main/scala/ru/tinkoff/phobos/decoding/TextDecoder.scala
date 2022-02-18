package ru.tinkoff.phobos.decoding

import java.time._
import java.util.{Base64, UUID}

import javax.xml.stream.XMLStreamConstants
import ru.tinkoff.phobos.decoding.TextDecoder.{EMappedDecoder, MappedDecoder}

import scala.annotation.tailrec

/** Warning! This is a complicated internal API which may change in future. Do not implement or use this trait directly
  * unless you know what you are doing.
  *
  * Use XmlDecoder for decoding XML documents.
  *
  * TextDecoder instance must exist for every type decoded from text inside XML element. This typeclass is used for
  * decoding case class parameters with @text annotation.
  *
  * To create new instance use .map or .emap method of existing instance.
  */
trait TextDecoder[A] { self =>
  def decodeAsText(c: Cursor): TextDecoder[A]
  def result(history: => List[String]): Either[DecodingError, A]
  def isCompleted: Boolean

  def map[B](f: A => B): TextDecoder[B] = new MappedDecoder(self, f)

  def emap[B](f: (List[String], A) => Either[DecodingError, B]): TextDecoder[B] = new EMappedDecoder(self, f)
}

object TextDecoder extends TextLiteralInstances {

  def apply[A](implicit instance: TextDecoder[A]) = instance

  class MappedDecoder[A, B](fa: TextDecoder[A], f: A => B) extends TextDecoder[B] {

    def decodeAsText(c: Cursor): TextDecoder[B] =
      new MappedDecoder[A, B](fa.decodeAsText(c), f)

    def result(history: => List[String]): Either[DecodingError, B] = fa.result(history).map(f)

    val isCompleted: Boolean = fa.isCompleted

    override def toString: String = s"MappedDecoder(${fa.toString})"
  }

  final class EMappedDecoder[A, B](fa: TextDecoder[A], f: (List[String], A) => Either[DecodingError, B])
      extends TextDecoder[B] {

    def decodeAsText(c: Cursor): TextDecoder[B] =
      new EMappedDecoder(fa.decodeAsText(c), f)

    def result(history: => List[String]): Either[DecodingError, B] = fa.result(history) match {
      case Right(a)    => f(history, a)
      case Left(error) => Left(error)
    }

    def isCompleted: Boolean = fa.isCompleted
  }

  final class ConstDecoder[A](a: A) extends TextDecoder[A] {
    def decodeAsText(c: Cursor): TextDecoder[A] = this

    def result(history: => List[String]): Either[DecodingError, A] = Right(a)

    val isCompleted: Boolean = true

    override def toString: String = s"ConstDecoder($a)"
  }

  final class FailedDecoder[A](decodingError: DecodingError) extends TextDecoder[A] {
    def decodeAsText(c: Cursor): TextDecoder[A] = this

    def result(history: => List[String]): Either[DecodingError, A] = Left(decodingError)

    val isCompleted: Boolean = true

    override def toString: String = s"FailedDecoder($decodingError)"
  }

  /** Instances
    */
  class StringDecoder(string: String = "") extends TextDecoder[String] {
    def decodeAsText(c: Cursor): TextDecoder[String] = {
      val stringBuilder = new StringBuilder(string)
      @tailrec
      def go(): TextDecoder[String] = {
        if (c.isCharacters || c.getEventType == XMLStreamConstants.CDATA) {
          stringBuilder.append(c.getText)
          c.next()
          go()
        } else {
          new StringDecoder(stringBuilder.mkString)
        }
      }
      go()
    }

    def result(history: => List[String]): Either[DecodingError, String] =
      Right(string)

    val isCompleted: Boolean = true

    override def toString: String = s"StringDecoder($string)"
  }

  implicit val stringDecoder: TextDecoder[String] = new StringDecoder()

  implicit val unitDecoder: TextDecoder[Unit] = new ConstDecoder[Unit](())

  implicit val booleanDecoder: TextDecoder[Boolean] =
    stringDecoder.emap((history, string) =>
      string match {
        case "true" | "1"  => Right(true)
        case "false" | "0" => Right(false)
        case str           => Left(DecodingError(s"Value `$str` is not `true` or `false`", history))
      },
    )

  implicit val javaBooleanDecoder: TextDecoder[java.lang.Boolean] = booleanDecoder.map(_.booleanValue())

  implicit val charDecoder: TextDecoder[Char] =
    stringDecoder.emap((history, string) => {
      if (string.length != 1) {
        Left(DecodingError("Value too long for char", history))
      } else {
        Right(string.head)
      }
    })
  implicit val javaCharacterDecoder: TextDecoder[java.lang.Character] = charDecoder.map(_.charValue())
  implicit val floatDecoder: TextDecoder[Float]                       = stringDecoder.emap(wrapException(_.toFloat))
  implicit val javaFloatDecoder: TextDecoder[java.lang.Float]         = floatDecoder.map(_.floatValue())
  implicit val doubleDecoder: TextDecoder[Double]                     = stringDecoder.emap(wrapException(_.toDouble))
  implicit val javaDoubleDecoder: TextDecoder[java.lang.Double]       = doubleDecoder.map(_.doubleValue())
  implicit val byteDecoder: TextDecoder[Byte]                         = stringDecoder.emap(wrapException(_.toByte))
  implicit val javaByteDecoder: TextDecoder[java.lang.Byte]           = byteDecoder.map(_.byteValue())
  implicit val shortDecoder: TextDecoder[Short]                       = stringDecoder.emap(wrapException(_.toShort))
  implicit val javaShortDecoder: TextDecoder[java.lang.Short]         = shortDecoder.map(_.shortValue())
  implicit val intDecoder: TextDecoder[Int]                           = stringDecoder.emap(wrapException(_.toInt))
  implicit val javaIntegerDecoder: TextDecoder[java.lang.Integer]     = intDecoder.map(_.intValue())
  implicit val longDecoder: TextDecoder[Long]                         = stringDecoder.emap(wrapException(_.toLong))
  implicit val javaLongDecoder: TextDecoder[java.lang.Long]           = longDecoder.map(_.longValue())
  implicit val bigIntDecoder: TextDecoder[BigInt]                     = stringDecoder.emap(wrapException(BigInt.apply))

  implicit val javaBigIntegerDecoder: TextDecoder[java.math.BigInteger] =
    stringDecoder.emap(wrapException(str => new java.math.BigInteger(str)))

  implicit val bigDecimalDecoder: TextDecoder[BigDecimal]               = stringDecoder.map(BigDecimal.apply)
  implicit val javaBigDecimalDecoder: TextDecoder[java.math.BigDecimal] = bigDecimalDecoder.map(_.bigDecimal)
  implicit val UUIDDecoder: TextDecoder[UUID]                           = stringDecoder.emap(wrapException(UUID.fromString))

  implicit val base64Decoder: TextDecoder[Array[Byte]] = stringDecoder.emap(wrapException(Base64.getDecoder.decode))

  implicit val localDateTimeDecoder: TextDecoder[LocalDateTime] =
    stringDecoder.emap(wrapException(LocalDateTime.parse))

  implicit val zonedDateTimeDecoder: TextDecoder[ZonedDateTime] =
    stringDecoder.emap(wrapException(ZonedDateTime.parse))

  implicit val offsetDateTimeDecoder: TextDecoder[OffsetDateTime] =
    zonedDateTimeDecoder.map(_.toOffsetDateTime)

  implicit val localDateDecoder: TextDecoder[LocalDate] =
    stringDecoder.emap(wrapException(LocalDate.parse))

  implicit val localTimeDecoder: TextDecoder[LocalTime] =
    stringDecoder.emap(wrapException(LocalTime.parse))
}
