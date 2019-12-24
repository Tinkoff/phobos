package ru.tinkoff.phobos.decoding

import java.time.{LocalDate, LocalDateTime, LocalTime, ZonedDateTime}
import java.util.{Base64, UUID}

import cats.Functor
import cats.data.{Chain, NonEmptyChain, NonEmptyList, NonEmptyVector}
import com.fasterxml.aalto.AsyncXMLStreamReader
import javax.xml.stream.XMLStreamConstants
import ru.tinkoff.phobos.decoding.ElementDecoder.{EMappedDecoder, MappedDecoder}

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

/**
  * Warning! This is a complicated internal API which may change in future.
  * Do not implement or use this trait directly unless you know what you are doing.
  *
  * Use XmlDecoder for decoding XML documents.
  *
  * ElementDecoder instance must exist for every type decoded from XML element.
  *
  * ElementDecoder instance can be created
  *  - from existing instance by using .map or .emap method (mostly used for "simple" types);
  *  - by macros from ru.tinkoff.phobos.derivation.semiauto package (for case classes and sealed traits).
  *
  *
  * This typeclass describes process of decoding some element to an A value. Name of the element is
  * not defined in typeclass, it should be passed in decodeAsElement method.
  */
trait ElementDecoder[A] { self =>
  def decodeAsElement(c: Cursor, localName: String, namespaceUri: Option[String]): ElementDecoder[A]
  def result(history: List[String]): Either[DecodingError, A]
  def isCompleted: Boolean

  def map[B](f: A => B): ElementDecoder[B] = new MappedDecoder(self, f)

  def emap[B](f: (List[String], A) => Either[DecodingError, B]): ElementDecoder[B] = new EMappedDecoder(self, f)
}

object ElementDecoder {

  def errorIfWrongName[A](c: Cursor, localName: String, namespaceUri: Option[String]): Option[FailedDecoder[A]] = {
    namespaceUri match {
      case _ if c.getLocalName != localName =>
        Some(new FailedDecoder(c.error(s"Invalid local name. Expected '$localName', but found '${c.getLocalName}'")))
      case Some(uri) if uri != c.getNamespaceURI =>
        Some(new FailedDecoder(c.error(s"Invalid namespace. Expected '$uri', but found '${c.getNamespaceURI}'")))
      case None if c.getNamespaceURI != "" =>
        Some(new FailedDecoder(c.error(s"Invalid namespace. Expected no namespace, but found '${c.getNamespaceURI}'")))
      case _ => None
    }
  }

  def isNil(c: Cursor): Boolean = {
    val nilIdx = c.getAttributeIndex("http://www.w3.org/2001/XMLSchema-instance", "nil")
    nilIdx > -1 && c.getAttributeValue(nilIdx) == "true"
  }

  final class MappedDecoder[A, B](fa: ElementDecoder[A], f: A => B) extends ElementDecoder[B] {
    def decodeAsElement(c: Cursor, localName: String, namespaceUri: Option[String]): ElementDecoder[B] =
      new MappedDecoder(fa.decodeAsElement(c, localName, namespaceUri), f)

    def result(history: List[String]): Either[DecodingError, B] = fa.result(history).map(f)

    val isCompleted: Boolean = fa.isCompleted

    override def toString: String = s"MappedDecoder(${fa.toString})"
  }

  final class EMappedDecoder[A, B](fa: ElementDecoder[A], f: (List[String], A) => Either[DecodingError, B])
      extends ElementDecoder[B] {
    def decodeAsElement(c: Cursor, localName: String, namespaceUri: Option[String]): ElementDecoder[B] =
      new EMappedDecoder(fa.decodeAsElement(c, localName, namespaceUri), f)

    def result(history: List[String]): Either[DecodingError, B] =
      fa.result(history) match {
        case Right(a)    => f(history, a)
        case Left(error) => Left(error)
      }

    def isCompleted: Boolean = fa.isCompleted
  }

  implicit val decoderFunctor: Functor[ElementDecoder] =
    new Functor[ElementDecoder] {
      def map[A, B](fa: ElementDecoder[A])(f: A => B): ElementDecoder[B] = fa.map(f)
    }

  final class ConstDecoder[A](a: A) extends ElementDecoder[A] {
    def decodeAsElement(c: Cursor, localName: String, namespaceUri: Option[String]): ElementDecoder[A] =
      new FailedDecoder[A](c.error("Element is already decoded (Most likely it occurred more than once)"))

    def result(history: List[String]): Either[DecodingError, A] = Right(a)

    val isCompleted: Boolean = true

    override def toString: String = s"ConstDecoder($a)"
  }

  final class FailedDecoder[A](decodingError: DecodingError) extends ElementDecoder[A] {
    def decodeAsElement(c: Cursor, localName: String, namespaceUri: Option[String]): ElementDecoder[A] = this

    def result(history: List[String]): Either[DecodingError, A] = Left(decodingError)

    val isCompleted: Boolean = true

    override def toString: String = s"FailedDecoder($decodingError)"
  }

  /**
    * Instances
    */
  final class StringDecoder(string: String = "") extends ElementDecoder[String] {
    def decodeAsElement(c: Cursor, localName: String, namespaceUri: Option[String]): ElementDecoder[String] = {
      val stringBuilder = new StringBuilder(string)

      @tailrec
      def go(): ElementDecoder[String] = {
        if (c.isCharacters || c.getEventType == XMLStreamConstants.CDATA) {
          stringBuilder.append(c.getText)
          c.next()
          go()
        } else if (c.isEndElement) {
          ElementDecoder.errorIfWrongName[String](c, localName, namespaceUri).getOrElse {
            c.next()
            new ConstDecoder(stringBuilder.mkString)
          }
        } else if (c.getEventType == AsyncXMLStreamReader.EVENT_INCOMPLETE) {
          c.next()
          new StringDecoder(stringBuilder.mkString)
        } else {
          new FailedDecoder(c.error(s"Unexpected event: '${c.getEventType}'"))
        }
      }

      if (c.isStartElement && stringBuilder.isEmpty) {
        ElementDecoder.errorIfWrongName[String](c, localName, namespaceUri).getOrElse {
          c.next()
          go()
        }
      } else {
        go()
      }
    }

    def result(history: List[String]): Either[DecodingError, String] =
      Left(DecodingError("Decoding not complete", history))

    val isCompleted: Boolean = false

    override def toString: String =
      s"StringDecoder($string)"
  }

  implicit val stringDecoder: ElementDecoder[String] = new StringDecoder()

  implicit val unitDecoder: ElementDecoder[Unit] = new ConstDecoder[Unit](())

  implicit val booleanDecoder: ElementDecoder[Boolean] =
    stringDecoder.emap((history, string) =>
      string match {
        case "true" | "1"  => Right(true)
        case "false" | "0" => Right(false)
        case str           => Left(DecodingError(s"Value `$str` is not `true` or `false`", history))
    })

  implicit val javaBooleanDecoder: ElementDecoder[java.lang.Boolean] = booleanDecoder.map(_.booleanValue())

  implicit val charDecoder: ElementDecoder[Char] =
    stringDecoder.emap((history, string) => {
      if (string.length != 1) {
        Left(DecodingError("Value too long for char", history))
      } else {
        Right(string.head)
      }
    })
  implicit val javaCharacterDecoder: ElementDecoder[java.lang.Character] = charDecoder.map(_.charValue())
  implicit val floatDecoder: ElementDecoder[Float]                       = stringDecoder.emap(wrapException(_.toFloat))
  implicit val javaFloatDecoder: ElementDecoder[java.lang.Float]         = floatDecoder.map(_.floatValue())
  implicit val doubleDecoder: ElementDecoder[Double]                     = stringDecoder.emap(wrapException(_.toDouble))
  implicit val javaDoubleDecoder: ElementDecoder[java.lang.Double]       = doubleDecoder.map(_.doubleValue())
  implicit val byteDecoder: ElementDecoder[Byte]                         = stringDecoder.emap(wrapException(_.toByte))
  implicit val javaByteDecoder: ElementDecoder[java.lang.Byte]           = byteDecoder.map(_.byteValue())
  implicit val shortDecoder: ElementDecoder[Short]                       = stringDecoder.emap(wrapException(_.toShort))
  implicit val javaShortDecoder: ElementDecoder[java.lang.Short]         = shortDecoder.map(_.shortValue())
  implicit val intDecoder: ElementDecoder[Int]                           = stringDecoder.emap(wrapException(_.toInt))
  implicit val javaIntegerDecoder: ElementDecoder[java.lang.Integer]     = intDecoder.map(_.intValue())
  implicit val longDecoder: ElementDecoder[Long]                         = stringDecoder.emap(wrapException(_.toLong))
  implicit val javaLongDecoder: ElementDecoder[java.lang.Long]           = longDecoder.map(_.longValue())
  implicit val bigIntDecoder: ElementDecoder[BigInt]                     = stringDecoder.emap(wrapException(BigInt.apply))

  implicit val javaBigIntegerDecoder: ElementDecoder[java.math.BigInteger] =
    stringDecoder.emap(wrapException(str => new java.math.BigInteger(str)))

  implicit val bigDecimalDecoder: ElementDecoder[BigDecimal]               = stringDecoder.map(BigDecimal.apply)
  implicit val javaBigDecimalDecoder: ElementDecoder[java.math.BigDecimal] = bigDecimalDecoder.map(_.bigDecimal)
  implicit val UUIDDecoder: ElementDecoder[UUID]                           = stringDecoder.emap(wrapException(UUID.fromString))

  implicit val base64Decoder: ElementDecoder[Array[Byte]] = stringDecoder.emap(wrapException(Base64.getDecoder.decode))

  implicit def optionDecoder[A](implicit decoder: ElementDecoder[A]): ElementDecoder[Option[A]] =
    new ElementDecoder[Option[A]] {
      def decodeAsElement(c: Cursor, localName: String, namespaceUri: Option[String]): ElementDecoder[Option[A]] = {
        if (c.isStartElement) {
          ElementDecoder.errorIfWrongName[Option[A]](c, localName, namespaceUri).getOrElse {
            if (ElementDecoder.isNil(c) || c.isEmptyElement) {
              c.next()
              new ConstDecoder(None)
            } else {
              decoder.map[Option[A]](a => Some(a)).decodeAsElement(c, localName, namespaceUri)
            }
          }
        } else {
          new FailedDecoder[Option[A]](c.error(s"Unexpected event: '${c.getEventType}'"))
        }
      }

      def result(history: List[String]): Either[DecodingError, Option[A]] = Right(None)

      val isCompleted: Boolean = true
    }

  implicit def someDecoder[A](implicit e: ElementDecoder[A]): ElementDecoder[Some[A]] = e.map(Some.apply)

  implicit val noneDecoder: ElementDecoder[None.type] = new ConstDecoder[None.type](None)

  class ListDecoder[A](list: List[A] = Nil, currentItemDecoderOpt: Option[ElementDecoder[A]] = None)(
      implicit itemDecoder: ElementDecoder[A])
      extends ElementDecoder[List[A]] {
    def decodeAsElement(cursor: Cursor, localName: String, namespaceUri: Option[String]): ElementDecoder[List[A]] = {
      if (cursor.getEventType == AsyncXMLStreamReader.EVENT_INCOMPLETE) {
        this
      } else {
        val listBuffer: ListBuffer[A] = ListBuffer.empty
        listBuffer.appendAll(list)

        @tailrec
        def go(currentItemDecoder: Option[ElementDecoder[A]]): ElementDecoder[List[A]] = {
          val decoder = currentItemDecoder.getOrElse(itemDecoder)

          if (currentItemDecoder.isDefined || (cursor.isStartElement && cursor.getLocalName == localName)) {
            if (currentItemDecoder.isEmpty && ElementDecoder.isNil(cursor)) {
              cursor.next()
              go(None)
            } else {
              val newDecoder = decoder.decodeAsElement(cursor, localName, namespaceUri)
              if (newDecoder.isCompleted) {
                newDecoder.result(cursor.history) match {
                  case Right(a) =>
                    listBuffer.append(a)
                    go(None)
                  case Left(err) =>
                    new FailedDecoder(err)
                }
              } else {
                new ListDecoder[A](listBuffer.toList, Some(newDecoder))
              }
            }
          } else if (cursor.getEventType == AsyncXMLStreamReader.EVENT_INCOMPLETE || cursor.isStartElement || cursor.isEndElement) {
            new ListDecoder[A](listBuffer.toList)
          } else {
            cursor.next()
            go(None)
          }
        }
        go(currentItemDecoderOpt)
      }
    }

    def result(history: List[String]): Either[DecodingError, List[A]] =
      if (currentItemDecoderOpt.isEmpty) {
        Right(list)
      } else {
        Left(DecodingError("Decoding not complete", history))
      }

    def isCompleted: Boolean = currentItemDecoderOpt.isEmpty

    override def toString: String =
      s"ListDecoder(${itemDecoder.toString})"
  }

  implicit def listDecoder[A](implicit decoder: ElementDecoder[A]): ElementDecoder[List[A]] = new ListDecoder[A]()

  implicit def seqDecoder[A](implicit decoder: ElementDecoder[A]): ElementDecoder[Seq[A]] =
    listDecoder[A].map(_.toSeq)

  implicit def setDecoder[A](implicit decoder: ElementDecoder[A]): ElementDecoder[Set[A]] =
    listDecoder[A].map(_.toSet)

  implicit def vectorDecoder[A](implicit decoder: ElementDecoder[A]): ElementDecoder[Vector[A]] =
    listDecoder[A].map(_.toVector)

  implicit def chainDecoder[A](implicit decoder: ElementDecoder[A]): ElementDecoder[Chain[A]] =
    listDecoder[A].map(Chain.fromSeq)

  implicit def nonEmptyListDecoder[A](implicit decoder: ElementDecoder[A]): ElementDecoder[NonEmptyList[A]] =
    listDecoder[A].emap(
      (history, list) =>
        NonEmptyList
          .fromList(list)
          .fold[Either[DecodingError, NonEmptyList[A]]](Left(DecodingError("List is empty", history)))(Right.apply)
    )

  implicit def nonEmptyVectorDecoder[A](implicit decoder: ElementDecoder[A]): ElementDecoder[NonEmptyVector[A]] =
    vectorDecoder[A].emap(
      (history, vector) =>
        NonEmptyVector
          .fromVector(vector)
          .fold[Either[DecodingError, NonEmptyVector[A]]](Left(DecodingError("Vector is empty", history)))(Right.apply)
    )

  implicit def nonEmptyChainDecoder[A](implicit decoder: ElementDecoder[A]): ElementDecoder[NonEmptyChain[A]] =
    chainDecoder[A].emap(
      (history, chain) =>
        NonEmptyChain
          .fromChain(chain)
          .fold[Either[DecodingError, NonEmptyChain[A]]](Left(DecodingError("Chain is empty", history)))(Right.apply)
    )

  implicit val localDateTimeDecoder: ElementDecoder[LocalDateTime] =
    stringDecoder.emap(wrapException(LocalDateTime.parse))

  implicit val zonedDateTimeDecoder: ElementDecoder[ZonedDateTime] =
    stringDecoder.emap(wrapException(ZonedDateTime.parse))

  implicit val localDateDecoder: ElementDecoder[LocalDate] =
    stringDecoder.emap(wrapException(LocalDate.parse))

  implicit val localTimeDecoder: ElementDecoder[LocalTime] =
    stringDecoder.emap(wrapException(LocalTime.parse))
}
