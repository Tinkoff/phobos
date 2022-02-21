package ru.tinkoff.phobos.encoding

import java.time._
import java.util.{Base64, UUID}
import java.time.format.DateTimeFormatter

/** Warning! This is an internal API which may change in future. Do not implement or use this trait directly unless you
  * know what you are doing.
  *
  * Use XmlEncoder for encoding.
  *
  * ElementEncoder instance must exist for every type encoded to XML element.
  *
  * ElementEncoder instance can be created
  *   - from existing instance by using .contramap (mostly used for "simple" types);
  *   - by macros from ru.tinkoff.phobos.derivation.semiauto package (for case classes and sealed traits).
  *
  * This typeclass describes process of encoding some A value to XML document. Name of the element is not defined in
  * typeclass, it should be passed in encodeAsElement method.
  */
trait ElementEncoder[A] { self =>
  def encodeAsElement(a: A, sw: PhobosStreamWriter, localName: String, namespaceUri: Option[String]): Unit

  def contramap[B](f: B => A): ElementEncoder[B] =
    new ElementEncoder[B] {
      def encodeAsElement(b: B, sw: PhobosStreamWriter, localName: String, namespaceUri: Option[String]): Unit =
        self.encodeAsElement(f(b), sw, localName, namespaceUri)
    }
}

object ElementEncoder extends ElementLiteralInstances {

  def apply[A](implicit instance: ElementEncoder[A]) = instance

  /** Instances
    */
  implicit val stringEncoder: ElementEncoder[String] =
    new ElementEncoder[String] {
      def encodeAsElement(a: String, sw: PhobosStreamWriter, localName: String, namespaceUri: Option[String]): Unit = {
        namespaceUri.fold(sw.writeStartElement(localName))(ns => sw.writeStartElement(ns, localName))
        sw.writeCharacters(a)
        sw.writeEndElement()
      }
    }

  implicit val unitEncoder: ElementEncoder[Unit] =
    new ElementEncoder[Unit] {
      def encodeAsElement(a: Unit, sw: PhobosStreamWriter, localName: String, namespaceUri: Option[String]): Unit = ()
    }

  implicit val booleanEncoder: ElementEncoder[Boolean]                     = stringEncoder.contramap(_.toString)
  implicit val javaBooleanEncoder: ElementEncoder[java.lang.Boolean]       = booleanEncoder.contramap(_.booleanValue())
  implicit val charEncoder: ElementEncoder[Char]                           = stringEncoder.contramap(_.toString)
  implicit val javaCharacterEncoder: ElementEncoder[java.lang.Character]   = charEncoder.contramap(_.charValue())
  implicit val floatEncoder: ElementEncoder[Float]                         = stringEncoder.contramap(_.toString)
  implicit val javaFloatEncoder: ElementEncoder[java.lang.Float]           = floatEncoder.contramap(_.floatValue())
  implicit val doubleEncoder: ElementEncoder[Double]                       = stringEncoder.contramap(_.toString)
  implicit val javaDoubleEncoder: ElementEncoder[java.lang.Double]         = doubleEncoder.contramap(_.doubleValue())
  implicit val byteEncoder: ElementEncoder[Byte]                           = stringEncoder.contramap(_.toString)
  implicit val javaByteEncoder: ElementEncoder[java.lang.Byte]             = byteEncoder.contramap(_.byteValue())
  implicit val shortEncoder: ElementEncoder[Short]                         = stringEncoder.contramap(_.toString)
  implicit val javaShortEncoder: ElementEncoder[java.lang.Short]           = shortEncoder.contramap(_.shortValue())
  implicit val intEncoder: ElementEncoder[Int]                             = stringEncoder.contramap(_.toString)
  implicit val javaIntegerEncoder: ElementEncoder[java.lang.Integer]       = intEncoder.contramap(_.intValue())
  implicit val longEncoder: ElementEncoder[Long]                           = stringEncoder.contramap(_.toString)
  implicit val javaLongEncoder: ElementEncoder[java.lang.Long]             = longEncoder.contramap(_.longValue())
  implicit val bigIntEncoder: ElementEncoder[BigInt]                       = stringEncoder.contramap(_.toString)
  implicit val javaBigIntegerEncoder: ElementEncoder[java.math.BigInteger] = bigIntEncoder.contramap(BigInt.apply)
  implicit val bigDecimalEncoder: ElementEncoder[BigDecimal]               = stringEncoder.contramap(_.toString)
  implicit val javaBigDecimalEncoder: ElementEncoder[java.math.BigDecimal] =
    bigDecimalEncoder.contramap(BigDecimal.apply)
  implicit val UUIDEncoder: ElementEncoder[UUID] = stringEncoder.contramap(_.toString)

  implicit val base64Encoder: ElementEncoder[Array[Byte]] = stringEncoder.contramap(Base64.getEncoder.encodeToString)

  implicit def optionEncoder[A](implicit encoder: ElementEncoder[A]): ElementEncoder[Option[A]] =
    new ElementEncoder[Option[A]] {
      def encodeAsElement(a: Option[A], sw: PhobosStreamWriter, localName: String, namespaceUri: Option[String]): Unit =
        a.foreach(encoder.encodeAsElement(_, sw, localName, namespaceUri))
    }

  implicit def someEncoder[A](implicit e: ElementEncoder[A]): ElementEncoder[Some[A]] = e.contramap(_.get)

  implicit val noneEncoder: ElementEncoder[None.type] = unitEncoder.contramap(_ => ())

  implicit def iteratorEncoder[A](implicit encoder: ElementEncoder[A]): ElementEncoder[Iterator[A]] =
    new ElementEncoder[Iterator[A]] {
      def encodeAsElement(
          as: Iterator[A],
          sw: PhobosStreamWriter,
          localName: String,
          namespaceUri: Option[String],
      ): Unit =
        as.foreach(a => encoder.encodeAsElement(a, sw, localName, namespaceUri))
    }

  implicit def seqEncoder[A](implicit encoder: ElementEncoder[A]): ElementEncoder[Seq[A]] =
    iteratorEncoder[A].contramap(_.iterator)

  implicit def setEncoder[A](implicit encoder: ElementEncoder[A]): ElementEncoder[Set[A]] =
    iteratorEncoder[A].contramap(_.iterator)

  implicit def listEncoder[A](implicit encoder: ElementEncoder[A]): ElementEncoder[List[A]] =
    iteratorEncoder[A].contramap(_.iterator)

  implicit def vectorEncoder[A](implicit encoder: ElementEncoder[A]): ElementEncoder[Vector[A]] =
    iteratorEncoder[A].contramap(_.iterator)

  implicit val localDateTimeEncoder: ElementEncoder[LocalDateTime] =
    stringEncoder.contramap(_.toString)

  def localDateTimeEncoderWithFormatter(formatter: DateTimeFormatter): ElementEncoder[LocalDateTime] =
    stringEncoder.contramap(_.format(formatter))

  implicit val zonedDateTimeEncoder: ElementEncoder[ZonedDateTime] =
    stringEncoder.contramap(_.toString)

  def zonedDateTimeEncoderWithFormatter(formatter: DateTimeFormatter): ElementEncoder[ZonedDateTime] =
    stringEncoder.contramap(_.format(formatter))

  implicit val offsetDateTimeEncoder: ElementEncoder[OffsetDateTime] =
    stringEncoder.contramap(_.toString)

  def offsetDateTimeEncoderWithFormatter(formatter: DateTimeFormatter): ElementEncoder[OffsetDateTime] =
    stringEncoder.contramap(_.format(formatter))

  implicit val localDateEncoder: ElementEncoder[LocalDate] =
    stringEncoder.contramap(_.toString)

  def localDateEncoderWithFormatter(formatter: DateTimeFormatter): ElementEncoder[LocalDate] =
    stringEncoder.contramap(_.format(formatter))

  implicit val localTimeEncoder: ElementEncoder[LocalTime] =
    stringEncoder.contramap(_.toString)

  def localTimeEncoderWithFormatter(formatter: DateTimeFormatter): ElementEncoder[LocalTime] =
    stringEncoder.contramap(_.format(formatter))
}
