package ru.tinkoff.phobos.fs2

import cats.data.NonEmptyList
import cats.effect.Sync
import fs2._
import ru.tinkoff.phobos.decoding._
import com.fasterxml.aalto.AsyncXMLStreamReader.EVENT_INCOMPLETE
import javax.xml.stream.XMLStreamConstants._
import scala.annotation.tailrec

object Parse {
  def oneDocument(rootElement: String) = OneDocument(NonEmptyList.one(rootElement))

  case class OneDocument(path: NonEmptyList[String]) {
    def inElement(elementName: String) = copy(path = elementName :: path)

    def everyElementAs[T: XmlDecoder] = OneDocumentDecoderApplied(path, XmlDecoder[T])
  }

  case class OneDocumentDecoderApplied[T](path: NonEmptyList[String], decoder: XmlDecoder[T]) {
    def toFs2Stream[F[_]: Sync](initialStream: Stream[F, Byte]): Stream[F, Either[DecodingError, T]] = {
      val path = this.path.toList

      val streamReader =
        Stream.bracket(Sync[F].pure(XmlDecoder.createStreamReader("UTF-8")))(reader => Sync[F].delay(reader.close()))

      streamReader.flatMap { sr =>
        val cursor      = new Cursor(sr)
        var lastDecoder = decoder.elementdecoder

        initialStream.chunks.noneTerminate.flatMap { available =>
          available match {
            case Some(chunk) => sr.getInputFeeder().feedInput(chunk.toArray, 0, chunk.size)
            case None        => sr.getInputFeeder().endOfInput()
          }

          // Move cursor until:
          // - EVENT_INCOMPLETE | END_DOCUMENT             -- here we have nothing to do but pull another chunk or terminate
          // - START_ELEMENT & cursor.history.tail == path -- stop when meet next element inside given path to decode
          // - _ & lastDecoder != decoder.elementdecoder   -- lastDecoder is partially filled so we have to proceed any further data
          @tailrec
          def skipUnnecessary(): Int =
            cursor.next() match {
              case ev @ (EVENT_INCOMPLETE | END_DOCUMENT)                                       => ev
              case ev @ START_ELEMENT if cursor.history.nonEmpty && cursor.history.tail == path => ev
              case ev if lastDecoder != decoder.elementdecoder                                  => ev
              case _                                                                            => skipUnnecessary()
            }

          skipUnnecessary() match {
            case EVENT_INCOMPLETE | END_DOCUMENT => Stream.emit(None)
            case _ =>
              Stream.unfoldLoop(lastDecoder) { d =>
                val newDecoder = d.decodeAsElement(cursor, decoder.localname, decoder.namespaceuri)

                if (!newDecoder.isCompleted) {
                  // Decoder has consumed all the available data although decoding isn't completed.
                  // We have to pull more data and proceed with this decoder.
                  lastDecoder = newDecoder
                  None -> None
                } else {
                  lastDecoder = decoder.elementdecoder
                  val result = newDecoder.result(cursor.history)

                  if (skipUnnecessary() == START_ELEMENT) Some(result) -> Some(lastDecoder)
                  else Some(result)                                    -> None
                }
              }
          }
        }.unNone
      }
    }
  }

}
