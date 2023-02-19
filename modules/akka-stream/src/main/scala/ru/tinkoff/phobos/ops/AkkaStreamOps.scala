package ru.tinkoff.phobos.ops

import akka.NotUsed
import akka.stream.scaladsl.{Flow, Keep, Sink}
import javax.xml.stream.XMLStreamConstants
import ru.tinkoff.phobos.decoding._
import scala.concurrent.Future

private[phobos] trait AkkaStreamOps {

  /** @note
    *   - works only for streams emmiting single xml document
    */
  def decodingFlow[A: XmlDecoder](charset: String = "UTF-8"): Flow[Array[Byte], Either[DecodingError, A], NotUsed] = {
    val xmlDecoder = XmlDecoder[A]
    Flow[Array[Byte]]
      .fold(Option.empty[SinkDecoderState[A]]) { (stateOpt, bytes) =>
        val state = stateOpt.getOrElse(
          SinkDecoderState.initial(xmlDecoder, charset),
        ) // trick to make this flow reusable (because of mutable Cursor)
        import state.{cursor, elementDecoder, xmlStreamReader}
        xmlStreamReader.getInputFeeder.feedInput(bytes, 0, bytes.length)
        cursor.next()
        while (
          cursor.getEventType == XMLStreamConstants.DTD || cursor.getEventType == XMLStreamConstants.START_DOCUMENT
        ) {
          cursor.next()
        }

        Some {
          state withEncoder {
            if (elementDecoder.result(cursor.history).isRight) {
              elementDecoder
            } else {
              elementDecoder.decodeAsElement(cursor, xmlDecoder.localname, xmlDecoder.namespaceuri)
            }
          }
        }
      }
      .map {
        case None =>
          throw DecodingError("Got an internal error while decoding byte stream", Nil, None)

        case Some(SinkDecoderState(_, cursor, elementDecoder)) =>
          elementDecoder.result(cursor.history)
      }
  }

  /** @note
    *   - works only for streams emmiting single xml document
    */
  def decodingFlowUnsafe[A: XmlDecoder](charset: String = "UTF-8"): Flow[Array[Byte], A, NotUsed] =
    decodingFlow(charset).map(_.fold(throw _, identity))

  def decodingSink[A: XmlDecoder](charset: String = "UTF-8"): Sink[Array[Byte], Future[Either[DecodingError, A]]] =
    decodingFlow(charset).toMat(Sink.head)(Keep.right)

  def decodingSinkUnsafe[A: XmlDecoder](charset: String = "UTF-8"): Sink[Array[Byte], Future[A]] =
    decodingFlowUnsafe(charset).toMat(Sink.head)(Keep.right)
}

private[phobos] case class SinkDecoderState[A](
    xmlStreamReader: XmlStreamReader,
    cursor: Cursor,
    elementDecoder: ElementDecoder[A],
) {
  def withEncoder(that: ElementDecoder[A]): SinkDecoderState[A] = copy(elementDecoder = that)
}

private[phobos] object SinkDecoderState {

  def initial[A](xmlDecoder: XmlDecoder[A], charset: String): SinkDecoderState[A] = {
    val sr: XmlStreamReader = XmlDecoder.createStreamReader(charset)
    val cursor              = new Cursor(sr)
    SinkDecoderState(
      xmlStreamReader = sr,
      cursor = cursor,
      elementDecoder = xmlDecoder.elementdecoder,
    )
  }
}
