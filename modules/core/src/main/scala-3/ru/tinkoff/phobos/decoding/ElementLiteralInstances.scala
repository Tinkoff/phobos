package ru.tinkoff.phobos.decoding

private[decoding] trait ElementLiteralInstances {
  implicit def literalDecoder[A, L <: A](implicit decoder: ElementDecoder[A], valueOfL: ValueOf[L]): ElementDecoder[L] =
    decoder
      .emap((history, a) =>
        if (a == valueOfL.value) Right(valueOfL.value)
        else
          Left(DecodingError(s"Failed to decode literal type. Expected: ${valueOfL.value}, actual: $a", history, None)),
      )
}
