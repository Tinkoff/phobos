package ru.tinkoff.phobos.encoding

import org.codehaus.stax2.XMLStreamWriter2

trait AttributeEncoderInstances {
  implicit val stringEncoder: AttributeEncoder[String] =
    new AttributeEncoder[String] {
      def encodeAsAttribute(a: String, sw: XMLStreamWriter2, localName: String, namespaceUri: Option[String]): Unit =
        namespaceUri.fold(sw.writeAttribute(localName, a))(ns => sw.writeAttribute(ns, localName, a))
    }

  implicit def valueEncoder[A](implicit encoder: ValueEncoder[A]): AttributeEncoder[A] =
    stringEncoder.contramap(encoder.encode)

  implicit def optionEncoder[A](implicit encoder: AttributeEncoder[A]): AttributeEncoder[Option[A]] =
    new AttributeEncoder[Option[A]] {
      def encodeAsAttribute(a: Option[A], sw: XMLStreamWriter2, localName: String, namespaceUri: Option[String]): Unit =
        a.foreach(encoder.encodeAsAttribute(_, sw, localName, namespaceUri))
    }
}
