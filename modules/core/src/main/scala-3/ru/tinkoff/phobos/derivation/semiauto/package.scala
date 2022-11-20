package ru.tinkoff.phobos.derivation

import ru.tinkoff.phobos.Namespace
import ru.tinkoff.phobos.configured.ElementCodecConfig
import ru.tinkoff.phobos.decoding.{ElementDecoder, XmlDecoder}
import ru.tinkoff.phobos.encoding.{ElementEncoder, XmlEncoder}

import scala.deriving.Mirror

package object semiauto {
  inline def deriveElementEncoder[T]: ElementEncoder[T] =
    encoder.deriveElementEncoder[T](ElementCodecConfig.default)
  inline def deriveElementEncoderConfigured[T](config: ElementCodecConfig): ElementEncoder[T] =
    encoder.deriveElementEncoder[T](config)
  inline def deriveXmlEncoder[T](localName: String): XmlEncoder[T] =
    encoder.deriveXmlEncoder[T](localName, None, None, ElementCodecConfig.default)
  inline def deriveXmlEncoderConfigured[T](localName: String, config: ElementCodecConfig): XmlEncoder[T] =
    encoder.deriveXmlEncoder[T](localName, None, None, config)
  inline def deriveXmlEncoder[T, NS: Namespace](localName: String, ns: NS): XmlEncoder[T] =
    encoder.deriveXmlEncoder[T](localName, Some(Namespace[NS].getNamespace), Namespace[NS].getPreferredPrefix, ElementCodecConfig.default)
  inline def deriveXmlEncoderConfigured[T, NS: Namespace](localName: String, ns: NS, config: ElementCodecConfig): XmlEncoder[T] =
    encoder.deriveXmlEncoder[T](localName, Some(Namespace[NS].getNamespace), Namespace[NS].getPreferredPrefix, config)

  inline def deriveElementDecoder[T]: ElementDecoder[T] =
    decoder.deriveElementDecoder[T](ElementCodecConfig.default)
  inline def deriveElementDecoderConfigured[T](config: ElementCodecConfig): ElementDecoder[T] =
    decoder.deriveElementDecoder[T](config)
  inline def deriveXmlDecoder[T](localName: String): XmlDecoder[T] =
    decoder.deriveXmlDecoder[T](localName, None, ElementCodecConfig.default)
  inline def deriveXmlDecoderConfigured[T](localName: String, config: ElementCodecConfig): XmlDecoder[T] =
    decoder.deriveXmlDecoder[T](localName, None, config)
  inline def deriveXmlDecoder[T, NS: Namespace](localName: String, ns: NS): XmlDecoder[T] =
    decoder.deriveXmlDecoder[T](localName, Some(Namespace[NS].getNamespace), ElementCodecConfig.default)
  inline def deriveXmlDecoderConfigured[T, NS: Namespace](localName: String, ns: NS, config: ElementCodecConfig): XmlDecoder[T] =
    decoder.deriveXmlDecoder[T](localName, Some(Namespace[NS].getNamespace), config)
}
