package ru.tinkoff.phobos.decoding

private[decoding] trait AttributeLiteralInstances {
  implicit def literalDecoder[A, L <: A](
      implicit decoder: AttributeDecoder[A],
      valueOfL: ValueOf[L],
  ): AttributeDecoder[L] =
    decoder
      .emap((history, a) =>
        if (a == valueOfL.value) Right(valueOfL.value)
        else
          Left(DecodingError(s"Failed to decode literal type. Expected: ${valueOfL.value}, actual: $a", history, None)),
      )
}
