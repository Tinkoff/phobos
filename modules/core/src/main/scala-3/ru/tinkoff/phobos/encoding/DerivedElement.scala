package ru.tinkoff.phobos.encoding

import ru.tinkoff.phobos.configured.ElementCodecConfig
import ru.tinkoff.phobos.derivation.encoder

private[encoding] trait DerivedElement {
  inline def derived[T]: ElementEncoder[T] =
    encoder.deriveElementEncoder[T](ElementCodecConfig.default)
}
