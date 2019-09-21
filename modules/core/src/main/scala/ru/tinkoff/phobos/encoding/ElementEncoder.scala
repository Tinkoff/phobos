package ru.tinkoff.phobos.encoding

import cats.Contravariant
import org.codehaus.stax2.XMLStreamWriter2

trait ElementEncoder[A] { self =>
  def encodeAsElement(a: A, sw: XMLStreamWriter2, localName: String, namespaceUri: Option[String]): Unit

  def contramap[B](f: B => A): ElementEncoder[B] =
    new ElementEncoder[B] {
      def encodeAsElement(b: B, sw: XMLStreamWriter2, localName: String, namespaceUri: Option[String]): Unit =
        self.encodeAsElement(f(b), sw, localName, namespaceUri)
    }
}

object ElementEncoder extends ElementEncoderInstances {
  implicit val encoderContravariant: Contravariant[ElementEncoder] =
    new Contravariant[ElementEncoder] {
      def contramap[A, B](fa: ElementEncoder[A])(f: B => A): ElementEncoder[B] = fa.contramap(f)
    }
}
