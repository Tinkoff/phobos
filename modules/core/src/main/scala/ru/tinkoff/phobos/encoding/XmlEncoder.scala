package ru.tinkoff.phobos.encoding

import java.io.ByteArrayOutputStream

import org.codehaus.stax2.XMLStreamWriter2
import cats.syntax.option._
import com.fasterxml.aalto.stax.OutputFactoryImpl
import ru.tinkoff.phobos.Namespace

trait XmlEncoder[A] {
  def encodeToBytes(a: A): Array[Byte]

  def encode(a: A): String =
    new String(encodeToBytes(a))
}

object XmlEncoder {

  def apply[A](implicit instance: XmlEncoder[A]): XmlEncoder[A] = instance

  def fromElementEncoder[A](localName: String, namespaceUri: Option[String])(
      implicit elementEncoder: ElementEncoder[A]): XmlEncoder[A] =
    new XmlEncoder[A] {
      def encodeToBytes(a: A): Array[Byte] = {
        val os      = new ByteArrayOutputStream
        val factory = new OutputFactoryImpl
        factory.setProperty("javax.xml.stream.isRepairingNamespaces", true)
        val sw = factory.createXMLStreamWriter(os, "UTF-8").asInstanceOf[XMLStreamWriter2]
        sw.writeStartDocument()
        elementEncoder.encodeAsElement(a, sw, localName, namespaceUri)
        sw.writeEndDocument()
        sw.flush()
        sw.close()
        os.toByteArray
      }
    }

  def fromElementEncoder[A](localName: String)(implicit elementEncoder: ElementEncoder[A]): XmlEncoder[A] =
    fromElementEncoder(localName, None)

  def fromElementEncoderNs[A, NS](localName: String, namespaceInstance: NS)(implicit elementEncoder: ElementEncoder[A],
                                                                            namespace: Namespace[NS]): XmlEncoder[A] =
    fromElementEncoder(localName, namespace.getNamespace.some)

  def fromElementEncoderNs[A, NS](localName: String)(implicit elementEncoder: ElementEncoder[A],
                                                     namespace: Namespace[NS]): XmlEncoder[A] =
    fromElementEncoder(localName, namespace.getNamespace.some)
}
