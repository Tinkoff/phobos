package ru.tinkoff.phobos.encoding

import java.time.{LocalDate, LocalDateTime, LocalTime, ZonedDateTime}
import java.util.{Base64, UUID}

import cats.Contravariant
import org.codehaus.stax2.XMLStreamWriter2

trait AttributeEncoder[A] { self =>
  def encodeAsAttribute(a: A, sw: XMLStreamWriter2, localName: String, namespaceUri: Option[String]): Unit

  def contramap[B](f: B => A): AttributeEncoder[B] =
    new AttributeEncoder[B] {
      def encodeAsAttribute(b: B, sw: XMLStreamWriter2, localName: String, namespaceUri: Option[String]): Unit =
        self.encodeAsAttribute(f(b), sw, localName, namespaceUri)
    }
}

object AttributeEncoder {
  implicit val encoderContravariant: Contravariant[AttributeEncoder] =
    new Contravariant[AttributeEncoder] {
      def contramap[A, B](fa: AttributeEncoder[A])(f: B => A): AttributeEncoder[B] = fa.contramap(f)
    }

  /**
    * Instances
    */
  implicit val stringEncoder: AttributeEncoder[String] =
    new AttributeEncoder[String] {
      def encodeAsAttribute(a: String, sw: XMLStreamWriter2, localName: String, namespaceUri: Option[String]): Unit =
        namespaceUri.fold(sw.writeAttribute(localName, a))(ns => sw.writeAttribute(ns, localName, a))
    }

  implicit val unitEncoder: AttributeEncoder[Unit] =
    new AttributeEncoder[Unit] {
      def encodeAsAttribute(a: Unit, sw: XMLStreamWriter2, localName: String, namespaceUri: Option[String]): Unit = ()
    }

  implicit val booleanEncoder: AttributeEncoder[Boolean]                     = stringEncoder.contramap(_.toString)
  implicit val javaBooleanEncoder: AttributeEncoder[java.lang.Boolean]       = booleanEncoder.contramap(_.booleanValue())
  implicit val charEncoder: AttributeEncoder[Char]                           = stringEncoder.contramap(_.toString)
  implicit val javaCharacterEncoder: AttributeEncoder[java.lang.Character]   = charEncoder.contramap(_.charValue())
  implicit val floatEncoder: AttributeEncoder[Float]                         = stringEncoder.contramap(_.toString)
  implicit val javaFloatEncoder: AttributeEncoder[java.lang.Float]           = floatEncoder.contramap(_.floatValue())
  implicit val doubleEncoder: AttributeEncoder[Double]                       = stringEncoder.contramap(_.toString)
  implicit val javaDoubleEncoder: AttributeEncoder[java.lang.Double]         = doubleEncoder.contramap(_.doubleValue())
  implicit val byteEncoder: AttributeEncoder[Byte]                           = stringEncoder.contramap(_.toString)
  implicit val javaByteEncoder: AttributeEncoder[java.lang.Byte]             = byteEncoder.contramap(_.byteValue())
  implicit val shortEncoder: AttributeEncoder[Short]                         = stringEncoder.contramap(_.toString)
  implicit val javaShortEncoder: AttributeEncoder[java.lang.Short]           = shortEncoder.contramap(_.shortValue())
  implicit val intEncoder: AttributeEncoder[Int]                             = stringEncoder.contramap(_.toString)
  implicit val javaIntegerEncoder: AttributeEncoder[java.lang.Integer]       = intEncoder.contramap(_.intValue())
  implicit val longEncoder: AttributeEncoder[Long]                           = stringEncoder.contramap(_.toString)
  implicit val javaLongEncoder: AttributeEncoder[java.lang.Long]             = longEncoder.contramap(_.longValue())
  implicit val bigIntEncoder: AttributeEncoder[BigInt]                       = stringEncoder.contramap(_.toString)
  implicit val javaBigIntegerEncoder: AttributeEncoder[java.math.BigInteger] = bigIntEncoder.contramap(BigInt.apply)
  implicit val bigDecimalEncoder: AttributeEncoder[BigDecimal]               = stringEncoder.contramap(_.toString)
  implicit val javaBigDecimalEncoder: AttributeEncoder[java.math.BigDecimal] =
    bigDecimalEncoder.contramap(BigDecimal.apply)
  implicit val UUIDEncoder: AttributeEncoder[UUID] = stringEncoder.contramap(_.toString)

  implicit val base64Encoder: AttributeEncoder[Array[Byte]] = stringEncoder.contramap(Base64.getEncoder.encodeToString)

  implicit def optionEncoder[A](implicit encoder: AttributeEncoder[A]): AttributeEncoder[Option[A]] =
    new AttributeEncoder[Option[A]] {
      def encodeAsAttribute(a: Option[A], sw: XMLStreamWriter2, localName: String, namespaceUri: Option[String]): Unit =
        a.foreach(encoder.encodeAsAttribute(_, sw, localName, namespaceUri))
    }

  implicit def someEncoder[A](implicit e: AttributeEncoder[A]): AttributeEncoder[Some[A]] = e.contramap(_.get)

  implicit val noneEncoder: AttributeEncoder[None.type] = unitEncoder.contramap(_ => ())

  implicit val localDateTimeEncoder: AttributeEncoder[LocalDateTime] =
    stringEncoder.contramap(_.toString)

  implicit val zonedDateTimeEncoder: AttributeEncoder[ZonedDateTime] =
    stringEncoder.contramap(_.toString)

  implicit val localDateEncoder: AttributeEncoder[LocalDate] =
    stringEncoder.contramap(_.toString)

  implicit val localTimeEncoder: AttributeEncoder[LocalTime] =
    stringEncoder.contramap(_.toString)
}
