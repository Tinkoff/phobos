package ru.tinkoff.phobos.decoding

trait ValueDecoder[A] {
  def decode(history: List[String], value: String): Either[DecodingError, A]
}

object ValueDecoder extends ValueDecoderInstances
