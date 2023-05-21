package ru.tinkoff.phobos.encoding

import ru.tinkoff.phobos.configured.ElementCodecConfig
import ru.tinkoff.phobos.derivation.encoder
import scala.deriving.Mirror
import ru.tinkoff.phobos.derivation.LazySummon

private[encoding] trait DerivedElement {
  inline def derived[T]: ElementEncoder[T] =
    encoder.deriveElementEncoder[T](ElementCodecConfig.default)

  inline given [T](using mirror: Mirror.Of[T]): LazySummon[ElementEncoder, T] = new:
    def instance = encoder.deriveElementEncoder[T](ElementCodecConfig.default)
}
