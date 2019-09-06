package ru.tinkoff.phobos.decoding

import com.fasterxml.aalto.AsyncXMLStreamReader
import javax.xml.stream.XMLStreamConstants
import ru.tinkoff.phobos.decoding.TextDecoder.FailedDecoder
import ru.tinkoff.phobos.decoding.TextDecoder.ConstDecoder

import scala.annotation.tailrec

trait TextDecoderInstances {

  class StringDecoder(string: String = "") extends TextDecoder[String] {
    def decodeAsText(c: Cursor, localName: String, namespaceUri: Option[String]): TextDecoder[String] = {
      val stringBuilder = new StringBuilder(string)
      @tailrec
      def go(): TextDecoder[String] = {
        if (c.isCharacters || c.getEventType == XMLStreamConstants.CDATA) {
          stringBuilder.append(c.getText)
          c.next()
          go()
        } else if (c.isStartElement) {
          if (c.getLocalName == localName) {
            c.next()
            go()
          } else {
            new StringDecoder(stringBuilder.mkString)
          }
        } else if (c.getEventType == AsyncXMLStreamReader.EVENT_INCOMPLETE) {
          c.next()
          new StringDecoder(stringBuilder.mkString)
        } else if (c.isEndElement) {
          new ConstDecoder(stringBuilder.mkString)
        } else {
          new FailedDecoder(c.error(s"Unexpected event: '${c.getEventType}'"))
        }
      }
      go()
    }

    def result(history: List[String]): Either[DecodingError, String] =
      Left(DecodingError("Decoding not complete", history))

    val isCompleted: Boolean = false

    override def toString: String = s"StringDecoder($string)"
  }

  implicit val stringDecoder: TextDecoder[String] = new StringDecoder()

  implicit def valueDecoder[A](implicit decoder: ValueDecoder[A]): TextDecoder[A] = stringDecoder.emap(decoder.decode)

  implicit def optionDecoder[A](implicit decoder: TextDecoder[A]): TextDecoder[Option[A]] =
    new TextDecoder[Option[A]] {
      def decodeAsText(c: Cursor, localName: String, namespaceUri: Option[String]): TextDecoder[Option[A]] = {
        if (c.isEndElement) {
          new ConstDecoder(None)
        } else {
          decoder.map[Option[A]](a => Some(a)).decodeAsText(c, localName, namespaceUri)
        }
      }

      def result(history: List[String]): Either[DecodingError, Option[A]] = Right(None)

      val isCompleted: Boolean = true
    }
}
