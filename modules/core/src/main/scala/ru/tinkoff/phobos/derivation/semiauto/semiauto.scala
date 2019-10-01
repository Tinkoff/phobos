package ru.tinkoff.phobos.derivation

import ru.tinkoff.phobos.decoding.{ElementDecoder, XmlDecoder}
import ru.tinkoff.phobos.encoding.{ElementEncoder, XmlEncoder}

package object semiauto {

  /**
   * Derives ElementEncoder for case class or sealed trait T. Usually, this macro
   * must be called for every case class or sealed trait contained in encoded ADT.
   *
   * Case class params are treated as
   *  - attributes, if they have @attr annotation (implicit AttributeEncoder is infered)
   *  - text inside element, if they have @text annotation (implicit TextEncoder is infered)
   *  - children elements, if they do not have these annotations (implicit ElementEncoder is infered)
   *
   *  Namespaces of elements and attributes are defined with @xmlns(nsi) annotation.
   *  In this case "nsi" inside @xmlns annotation is some case object with
   *  existing implicit ru.tinkoff.phobos.Namespace[nsi.type] instance.
   */
  def deriveElementEncoder[T]: ElementEncoder[T] = macro EncoderDerivation.element[T]

  /**
   * Creates XmlEncoder[T] by deriving ElementEncoder[T] and using XmlEncoder.fromElementEncoder(localName) function.
   * Warning: This macro does not makes ElementEncoder[T] instance visible. If you need ElementEncoder[T], use
   * deriveElementEncoder[T] and XmlEncoder.fromElementEncoder separately.
   */
  def deriveXmlEncoder[T](localName: String): XmlEncoder[T] = macro EncoderDerivation.xml[T]

  /**
   * Like deriveXmlEncoder(localName: String), but also provides namespace
   */
  def deriveXmlEncoder[T, NS](localName: String, ns: NS): XmlEncoder[T] = macro EncoderDerivation.xmlNs[T, NS]


  /**
   * Derives ElementDecoder for case class or sealed trait T. Usually, this macro
   * must be called for every case class or sealed trait contained in decoded ADT.
   *
   * Case class params are treated as
   *  - attributes, if they have @attr annotation (implicit AttributeDecoder is infered)
   *  - text inside element, if they have @text annotation (implicit TextDecoder is infered)
   *  - children elements, if they do not have these annotations (implicit ElementDecoder is infered)
   *
   *  Namespaces of elements and attributes are defined with @xmlns(nsi) annotation.
   *  In this case "nsi" inside @xmlns annotation is some case object with
   *  existing implicit ru.tinkoff.phobos.Namespace[nsi.type] instance.
   */
  def deriveElementDecoder[T]: ElementDecoder[T] = macro DecoderDerivation.element[T]

  /**
   * Creates XmlDecoder[T] by deriving ElementDecoder[T] and using XmlDecoder.fromElementDecoder(localName) function.
   * Warning: This macro does not makes ElementDecoder[T] instance visible. If you need ElementDecoder[T], use
   * deriveElementDecoder[T] and XmlDecoder.fromElementDecoder separately.
   */
  def deriveXmlDecoder[T](localName: String): XmlDecoder[T] = macro DecoderDerivation.xml[T]

  /**
   * Like deriveXmlDecoder(localName: String), but also provides namespace
   */
  def deriveXmlDecoder[T, NS](localName: String, ns: NS): XmlDecoder[T] = macro DecoderDerivation.xmlNs[T, NS]
}
