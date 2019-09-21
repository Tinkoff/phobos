package ru.tinkoff.phobos.derivation

import ru.tinkoff.phobos.decoding.{ElementDecoder, XmlDecoder}
import ru.tinkoff.phobos.encoding.{ElementEncoder, XmlEncoder}

package object semiauto {

  def deriveElementEncoder[T]: ElementEncoder[T] = macro EncoderDerivation.element[T]
  def deriveXmlEncoder[T](localName: String): XmlEncoder[T] = macro EncoderDerivation.xml[T]
  def deriveXmlEncoder[T, NS](localName: String, ns: NS): XmlEncoder[T] = macro EncoderDerivation.xmlNs[T, NS]

  def deriveElementDecoder[T]: ElementDecoder[T] = macro DecoderDerivation.element[T]
  def deriveXmlDecoder[T](localName: String): XmlDecoder[T] = macro DecoderDerivation.xml[T]
  def deriveXmlDecoder[T, NS](localName: String, ns: NS): XmlDecoder[T] = macro DecoderDerivation.xmlNs[T, NS]

}
