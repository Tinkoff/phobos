package ru.tinkoff.phobos.encoding

import org.codehaus.stax2.XMLStreamWriter2

trait ElementEncoderInstances {
  implicit val stringEncoder: ElementEncoder[String] =
    new ElementEncoder[String] {
      def encodeAsElement(a: String, sw: XMLStreamWriter2, localName: String, namespaceUri: Option[String]): Unit = {
        namespaceUri.fold(sw.writeStartElement(localName))(ns => sw.writeStartElement(ns, localName))
        sw.writeRaw(a)
        sw.writeEndElement()
      }
    }

  implicit def valueEncoder[A](implicit encoder: ValueEncoder[A]): ElementEncoder[A] =
    stringEncoder.contramap(encoder.encode)

  implicit def optionEncoder[A](implicit encoder: ElementEncoder[A]): ElementEncoder[Option[A]] =
    new ElementEncoder[Option[A]] {
      def encodeAsElement(a: Option[A], sw: XMLStreamWriter2, localName: String, namespaceUri: Option[String]): Unit =
        a.foreach(encoder.encodeAsElement(_, sw, localName, namespaceUri))
    }

  implicit def listEncoder[A](implicit encoder: ElementEncoder[A]): ElementEncoder[List[A]] =
    new ElementEncoder[List[A]] {
      def encodeAsElement(as: List[A], sw: XMLStreamWriter2, localName: String, namespaceUri: Option[String]): Unit =
        for (a <- as) encoder.encodeAsElement(a, sw, localName, namespaceUri)
    }
}
