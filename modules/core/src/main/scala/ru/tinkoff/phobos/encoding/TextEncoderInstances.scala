package ru.tinkoff.phobos.encoding

import org.codehaus.stax2.XMLStreamWriter2

trait TextEncoderInstances {
  implicit val stringEncoder: TextEncoder[String] =
    new TextEncoder[String] {
      def encodeAsText(a: String, sw: XMLStreamWriter2): Unit = sw.writeRaw(a)
    }

  implicit def valueEncoder[A](implicit encoder: ValueEncoder[A]): TextEncoder[A] =
    stringEncoder.contramap(encoder.encode)

}
