package ru.tinkoff.phobos.refined.encoding

import eu.timepit.refined.api.RefType
import ru.tinkoff.phobos.encoding._

trait EncodingInstances {

  implicit def refinedAttributeEncoder[F[_, _], T, P](
      implicit underlying: AttributeEncoder[T],
      refType: RefType[F],
  ): AttributeEncoder[F[T, P]] =
    underlying.contramap(refType.unwrap)

  implicit def refinedTextEncoder[F[_, _], T, P](
      implicit underlying: TextEncoder[T],
      refType: RefType[F],
  ): TextEncoder[F[T, P]] =
    underlying.contramap(refType.unwrap)

  implicit def refinedElementEncoder[F[_, _], T, P](
      implicit underlying: ElementEncoder[T],
      refType: RefType[F],
  ): ElementEncoder[F[T, P]] =
    underlying.contramap(refType.unwrap)
}
