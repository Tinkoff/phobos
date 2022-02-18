package ru.tinkoff.phobos.encoding

import java.time._
import java.util.{Base64, UUID}

/** Warning! This is an internal API which may change in future. Do not implement or use this trait directly unless you
  * know what you are doing.
  *
  * Use XmlEncoder for encoding XML documents.
  *
  * AttributeEncoder instance must exist for every type encoded to attribute. This typeclass is used for encoding case
  * class parameters with @attr annotation.
  *
  * To create new instance use .contramap method of existing instance.
  */
trait AttributeEncoder[A] { self =>
  def encodeAsAttribute(a: A, sw: PhobosStreamWriter, localName: String, namespaceUri: Option[String]): Unit

  def contramap[B](f: B => A): AttributeEncoder[B] =
    new AttributeEncoder[B] {
      def encodeAsAttribute(b: B, sw: PhobosStreamWriter, localName: String, namespaceUri: Option[String]): Unit =
        self.encodeAsAttribute(f(b), sw, localName, namespaceUri)
    }
}

object AttributeEncoder extends AttributeLiteralInstances {

  def apply[A](implicit instance: AttributeEncoder[A]) = instance

  /** Instances
    */
  implicit val stringEncoder: AttributeEncoder[String] =
    new AttributeEncoder[String] {
      def encodeAsAttribute(a: String, sw: PhobosStreamWriter, localName: String, namespaceUri: Option[String]): Unit =
        namespaceUri.fold(sw.writeAttribute(localName, a))(ns => sw.writeAttribute(ns, localName, a))
    }

  implicit val unitEncoder: AttributeEncoder[Unit] =
    new AttributeEncoder[Unit] {
      def encodeAsAttribute(a: Unit, sw: PhobosStreamWriter, localName: String, namespaceUri: Option[String]): Unit = ()
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
      def encodeAsAttribute(
          a: Option[A],
          sw: PhobosStreamWriter,
          localName: String,
          namespaceUri: Option[String],
      ): Unit =
        a.foreach(encoder.encodeAsAttribute(_, sw, localName, namespaceUri))
    }

  implicit def someEncoder[A](implicit e: AttributeEncoder[A]): AttributeEncoder[Some[A]] = e.contramap(_.get)

  implicit val noneEncoder: AttributeEncoder[None.type] = unitEncoder.contramap(_ => ())

  implicit val localDateTimeEncoder: AttributeEncoder[LocalDateTime] =
    stringEncoder.contramap(_.toString)

  implicit val zonedDateTimeEncoder: AttributeEncoder[ZonedDateTime] =
    stringEncoder.contramap(_.toString)

  implicit val offsetDateTimeEncoder: AttributeEncoder[OffsetDateTime] =
    zonedDateTimeEncoder.contramap(_.toZonedDateTime)

  implicit val localDateEncoder: AttributeEncoder[LocalDate] =
    stringEncoder.contramap(_.toString)

  implicit val localTimeEncoder: AttributeEncoder[LocalTime] =
    stringEncoder.contramap(_.toString)
}
