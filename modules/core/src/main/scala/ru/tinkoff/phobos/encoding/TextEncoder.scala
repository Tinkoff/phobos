package ru.tinkoff.phobos.encoding

import java.time._
import java.util.{Base64, UUID}

/** Use XmlEncoder for encoding XML documents.
  *
  * TextEncoder instance must exist for every type encoded to text inside XML element. This typeclass is used for
  * encoding case class parameters with @text annotation.
  *
  * To create new instance use .contramap method of existing instance.
  */

trait TextEncoder[A] { self =>
  def encodeAsText(a: A, sw: PhobosStreamWriter): Unit

  def contramap[B](f: B => A): TextEncoder[B] =
    new TextEncoder[B] {
      def encodeAsText(b: B, sw: PhobosStreamWriter): Unit = self.encodeAsText(f(b), sw)
    }
}

object TextEncoder extends TextLiteralInstances {

  def apply[A](implicit instance: TextEncoder[A]) = instance

  /** Instances
    */
  implicit val stringEncoder: TextEncoder[String] =
    new TextEncoder[String] {
      def encodeAsText(a: String, sw: PhobosStreamWriter): Unit = sw.writeRaw(a)
    }

  implicit val unitEncoder: TextEncoder[Unit] =
    new TextEncoder[Unit] {
      def encodeAsText(a: Unit, sw: PhobosStreamWriter): Unit = ()
    }

  implicit val booleanEncoder: TextEncoder[Boolean]                     = stringEncoder.contramap(_.toString)
  implicit val javaBooleanEncoder: TextEncoder[java.lang.Boolean]       = booleanEncoder.contramap(_.booleanValue())
  implicit val charEncoder: TextEncoder[Char]                           = stringEncoder.contramap(_.toString)
  implicit val javaCharacterEncoder: TextEncoder[java.lang.Character]   = charEncoder.contramap(_.charValue())
  implicit val floatEncoder: TextEncoder[Float]                         = stringEncoder.contramap(_.toString)
  implicit val javaFloatEncoder: TextEncoder[java.lang.Float]           = floatEncoder.contramap(_.floatValue())
  implicit val doubleEncoder: TextEncoder[Double]                       = stringEncoder.contramap(_.toString)
  implicit val javaDoubleEncoder: TextEncoder[java.lang.Double]         = doubleEncoder.contramap(_.doubleValue())
  implicit val byteEncoder: TextEncoder[Byte]                           = stringEncoder.contramap(_.toString)
  implicit val javaByteEncoder: TextEncoder[java.lang.Byte]             = byteEncoder.contramap(_.byteValue())
  implicit val shortEncoder: TextEncoder[Short]                         = stringEncoder.contramap(_.toString)
  implicit val javaShortEncoder: TextEncoder[java.lang.Short]           = shortEncoder.contramap(_.shortValue())
  implicit val intEncoder: TextEncoder[Int]                             = stringEncoder.contramap(_.toString)
  implicit val javaIntegerEncoder: TextEncoder[java.lang.Integer]       = intEncoder.contramap(_.intValue())
  implicit val longEncoder: TextEncoder[Long]                           = stringEncoder.contramap(_.toString)
  implicit val javaLongEncoder: TextEncoder[java.lang.Long]             = longEncoder.contramap(_.longValue())
  implicit val bigIntEncoder: TextEncoder[BigInt]                       = stringEncoder.contramap(_.toString)
  implicit val javaBigIntegerEncoder: TextEncoder[java.math.BigInteger] = bigIntEncoder.contramap(BigInt.apply)
  implicit val bigDecimalEncoder: TextEncoder[BigDecimal]               = stringEncoder.contramap(_.toString)
  implicit val javaBigDecimalEncoder: TextEncoder[java.math.BigDecimal] =
    bigDecimalEncoder.contramap(BigDecimal.apply)
  implicit val UUIDEncoder: TextEncoder[UUID] = stringEncoder.contramap(_.toString)

  implicit val base64Encoder: TextEncoder[Array[Byte]] = stringEncoder.contramap(Base64.getEncoder.encodeToString)

  implicit val localDateTimeEncoder: TextEncoder[LocalDateTime] =
    stringEncoder.contramap(_.toString)

  implicit val zonedDateTimeEncoder: TextEncoder[ZonedDateTime] =
    stringEncoder.contramap(_.toString)

  implicit val offsetDateTimeEncoder: TextEncoder[OffsetDateTime] =
    zonedDateTimeEncoder.contramap(_.toZonedDateTime)

  implicit val localDateEncoder: TextEncoder[LocalDate] =
    stringEncoder.contramap(_.toString)

  implicit val localTimeEncoder: TextEncoder[LocalTime] =
    stringEncoder.contramap(_.toString)
}
