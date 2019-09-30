package ru.tinkoff.phobos.encoding

import java.time.{LocalDate, LocalDateTime, LocalTime, ZonedDateTime}
import java.util.{Base64, UUID}

import cats.{Contravariant, Foldable}
import cats.data.{Chain, NonEmptyChain, NonEmptyList, NonEmptySet, NonEmptyVector}
import org.codehaus.stax2.XMLStreamWriter2

trait ElementEncoder[A] { self =>
  def encodeAsElement(a: A, sw: XMLStreamWriter2, localName: String, namespaceUri: Option[String]): Unit

  def contramap[B](f: B => A): ElementEncoder[B] =
    new ElementEncoder[B] {
      def encodeAsElement(b: B, sw: XMLStreamWriter2, localName: String, namespaceUri: Option[String]): Unit =
        self.encodeAsElement(f(b), sw, localName, namespaceUri)
    }
}

object ElementEncoder {
  implicit val encoderContravariant: Contravariant[ElementEncoder] =
    new Contravariant[ElementEncoder] {
      def contramap[A, B](fa: ElementEncoder[A])(f: B => A): ElementEncoder[B] = fa.contramap(f)
    }

  /**
    * Instances
    */
  implicit val stringEncoder: ElementEncoder[String] =
    new ElementEncoder[String] {
      def encodeAsElement(a: String, sw: XMLStreamWriter2, localName: String, namespaceUri: Option[String]): Unit = {
        namespaceUri.fold(sw.writeStartElement(localName))(ns => sw.writeStartElement(ns, localName))
        sw.writeRaw(a)
        sw.writeEndElement()
      }
    }

  implicit val unitEncoder: ElementEncoder[Unit] =
    new ElementEncoder[Unit] {
      def encodeAsElement(a: Unit, sw: XMLStreamWriter2, localName: String, namespaceUri: Option[String]): Unit = ()
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
      def encodeAsElement(a: Option[A], sw: XMLStreamWriter2, localName: String, namespaceUri: Option[String]): Unit =
        a.foreach(encoder.encodeAsElement(_, sw, localName, namespaceUri))
    }

  implicit def someEncoder[A](implicit e: ElementEncoder[A]): ElementEncoder[Some[A]] = e.contramap(_.get)

  implicit val noneEncoder: ElementEncoder[None.type] = unitEncoder.contramap(_ => ())

  implicit def foldableEncoder[F[_]: Foldable, A](implicit encoder: ElementEncoder[A]): ElementEncoder[F[A]] =
    new ElementEncoder[F[A]] {
      def encodeAsElement(as: F[A], sw: XMLStreamWriter2, localName: String, namespaceUri: Option[String]): Unit =
        Foldable[F].foldLeft(as, ())((_, a) => encoder.encodeAsElement(a, sw, localName, namespaceUri))
    }

  implicit def iteratorEncoder[A](implicit encoder: ElementEncoder[A]): ElementEncoder[Iterator[A]] =
    new ElementEncoder[Iterator[A]] {
      def encodeAsElement(as: Iterator[A],
                          sw: XMLStreamWriter2,
                          localName: String,
                          namespaceUri: Option[String]): Unit =
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

  implicit def chainEncoder[A](implicit encoder: ElementEncoder[A]): ElementEncoder[Chain[A]] =
    iteratorEncoder[A].contramap(_.iterator)

  implicit def nonEmptyListEncoder[A](implicit encoder: ElementEncoder[A]): ElementEncoder[NonEmptyList[A]] =
    listEncoder[A].contramap(_.toList)

  implicit def nonEmptyVectorEncoder[A](implicit encoder: ElementEncoder[A]): ElementEncoder[NonEmptyVector[A]] =
    vectorEncoder[A].contramap(_.toVector)

  implicit def nonEmptySetEncoder[A](implicit encoder: ElementEncoder[A]): ElementEncoder[NonEmptySet[A]] =
    setEncoder[A].contramap(_.toSortedSet)

  implicit def nonEmptyChainEncoder[A](implicit encoder: ElementEncoder[A]): ElementEncoder[NonEmptyChain[A]] =
    chainEncoder[A].contramap(_.toChain)

  implicit val localDateTimeEncoder: ElementEncoder[LocalDateTime] =
    stringEncoder.contramap(_.toString)

  implicit val zonedDateTimeEncoder: ElementEncoder[ZonedDateTime] =
    stringEncoder.contramap(_.toString)

  implicit val localDateEncoder: ElementEncoder[LocalDate] =
    stringEncoder.contramap(_.toString)

  implicit val localTimeEncoder: ElementEncoder[LocalTime] =
    stringEncoder.contramap(_.toString)
}
