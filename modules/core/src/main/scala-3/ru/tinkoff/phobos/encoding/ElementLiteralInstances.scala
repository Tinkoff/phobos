package ru.tinkoff.phobos.encoding

private[encoding] trait ElementLiteralInstances {
  implicit def literalEncoder[A, L <: A](implicit encoder: ElementEncoder[A], valueOfL: ValueOf[L]): ElementEncoder[L] =
    encoder.contramap(identity)
}
