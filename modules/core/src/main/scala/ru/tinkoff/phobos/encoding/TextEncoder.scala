package ru.tinkoff.phobos.encoding

import cats.Contravariant
import org.codehaus.stax2.XMLStreamWriter2

trait TextEncoder[A] { self =>
  def encodeAsText(a: A, sw: XMLStreamWriter2): Unit

  def contramap[B](f: B => A): TextEncoder[B] =
    new TextEncoder[B] {
      def encodeAsText(b: B, sw: XMLStreamWriter2): Unit = self.encodeAsText(f(b), sw)
    }
}

object TextEncoder extends TextEncoderInstances {
  implicit val encoderContravariant: Contravariant[TextEncoder] =
    new Contravariant[TextEncoder] {
      def contramap[A, B](fa: TextEncoder[A])(f: B => A): TextEncoder[B] = fa.contramap(f)
    }
}
