package ru.tinkoff.phobos.encoding

private[encoding] trait AttributeLiteralInstances {
  implicit def literalEncoder[A, L <: A](
      implicit encoder: AttributeEncoder[A],
      valueOfL: ValueOf[L],
  ): AttributeEncoder[L] =
    encoder.contramap(identity)
}
