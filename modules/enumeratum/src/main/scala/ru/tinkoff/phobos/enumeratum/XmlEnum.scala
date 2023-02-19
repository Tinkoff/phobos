package ru.tinkoff.phobos.enumeratum

import enumeratum.{Enum, EnumEntry}
import ru.tinkoff.phobos.decoding.{AttributeDecoder, DecodingError, ElementDecoder, TextDecoder}
import ru.tinkoff.phobos.encoding.{AttributeEncoder, ElementEncoder, TextEncoder}

trait XmlEnum[A <: EnumEntry] { this: Enum[A] =>
  implicit val enumElementEncoder: ElementEncoder[A]     = ElementEncoder.stringEncoder.contramap(a => a.entryName)
  implicit val enumAttributeEncoder: AttributeEncoder[A] = AttributeEncoder.stringEncoder.contramap(a => a.entryName)
  implicit val enumTextEncoder: TextEncoder[A]           = TextEncoder.stringEncoder.contramap(a => a.entryName)

  def decodeFromString(history: List[String], str: String): Either[DecodingError, A] = this.withNameOption(str) match {
    case Some(member) => Right(member)
    case _            => Left(DecodingError(s"'$str' in not a member of enum $this", history, None))
  }

  implicit val enumElementDecoder: ElementDecoder[A]     = ElementDecoder.stringDecoder.emap(decodeFromString)
  implicit val enumAttributeDecoder: AttributeDecoder[A] = AttributeDecoder.stringDecoder.emap(decodeFromString)
  implicit val enumTextDecoder: TextDecoder[A]           = TextDecoder.stringDecoder.emap(decodeFromString)
}
