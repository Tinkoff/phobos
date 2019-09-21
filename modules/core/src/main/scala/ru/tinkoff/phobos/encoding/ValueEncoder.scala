package ru.tinkoff.phobos.encoding

trait ValueEncoder[A] {
  def encode(value: A): String
}

object ValueEncoder extends ValueEncoderInstances
