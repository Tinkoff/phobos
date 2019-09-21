package ru.tinkoff.phobos.decoding

import cats.syntax.option._
import com.fasterxml.aalto.AsyncXMLStreamReader
import javax.xml.stream.XMLStreamConstants
import ru.tinkoff.phobos.decoding.ElementDecoder.{ConstDecoder, FailedDecoder}

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

trait ElementDecoderInstances {

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

  implicit def valueDecoder[A](implicit decoder: ValueDecoder[A]): ElementDecoder[A] =
    stringDecoder.emap(decoder.decode)

  implicit def optionDecoder[A](implicit decoder: ElementDecoder[A]): ElementDecoder[Option[A]] =
    new ElementDecoder[Option[A]] {
      def decodeAsElement(c: Cursor,
                          localName: String,
                          namespaceUri: Option[String]): ElementDecoder[Option[A]] = {
        if (c.isStartElement && c.getLocalName == localName) {
          if (ElementDecoder.isNil(c)) {
            c.next()
            new ConstDecoder(None)
          } else {
            decoder.map[Option[A]](a => Some(a)).decodeAsElement(c, localName, namespaceUri)
          }
        } else {
          new FailedDecoder[Option[A]](DecodingError("Wrong state", List()))
        }
      }

      def result(history: List[String]): Either[DecodingError, Option[A]] = Right(None)

      val isCompleted: Boolean = true
    }

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
                new ListDecoder[A](listBuffer.toList, newDecoder.some)
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
}
