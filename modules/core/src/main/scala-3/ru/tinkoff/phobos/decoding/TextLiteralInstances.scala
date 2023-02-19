package ru.tinkoff.phobos.decoding

private[decoding] trait TextLiteralInstances {
  implicit def literalDecoder[A, L <: A](implicit decoder: TextDecoder[A], valueOfL: ValueOf[L]): TextDecoder[L] =
    decoder
      .emap((history, a) =>
        if (a == valueOfL.value) Right(valueOfL.value)
        else Left(DecodingError(s"Failed to decode literal type. Expected: ${valueOfL.value}, actual: $a", history, None)),
      )
}
