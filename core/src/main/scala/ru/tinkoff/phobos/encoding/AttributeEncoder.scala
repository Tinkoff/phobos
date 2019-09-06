package ru.tinkoff.phobos.encoding

import cats.Contravariant
import org.codehaus.stax2.XMLStreamWriter2

trait AttributeEncoder[A] { self =>
  def encodeAsAttribute(a: A, sw: XMLStreamWriter2, localName: String, namespaceUri: Option[String] = None): Unit

  def contramap[B](f: B => A): AttributeEncoder[B] =
    new AttributeEncoder[B] {
      def encodeAsAttribute(b: B, sw: XMLStreamWriter2, localName: String, namespaceUri: Option[String]): Unit =
        self.encodeAsAttribute(f(b), sw, localName, namespaceUri)
    }
}

object AttributeEncoder extends AttributeEncoderInstances {
  implicit val encoderContravariant: Contravariant[AttributeEncoder] =
    new Contravariant[AttributeEncoder] {
      def contramap[A, B](fa: AttributeEncoder[A])(f: B => A): AttributeEncoder[B] = fa.contramap(f)
    }
}
