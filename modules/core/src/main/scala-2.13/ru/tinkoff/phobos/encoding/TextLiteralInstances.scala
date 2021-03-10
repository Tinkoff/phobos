package ru.tinkoff.phobos.encoding

private[encoding] trait TextLiteralInstances {
  implicit def literalEncoder[A, L <: A](implicit encoder: TextEncoder[A], valueOfL: ValueOf[L]): TextEncoder[L] =
    encoder.contramap(identity)
}
