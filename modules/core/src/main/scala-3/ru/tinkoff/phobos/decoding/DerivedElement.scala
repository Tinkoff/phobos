package ru.tinkoff.phobos.decoding

import ru.tinkoff.phobos.configured.ElementCodecConfig
import ru.tinkoff.phobos.derivation.decoder

private[decoding] trait DerivedElement {
  inline def derived[T]: ElementDecoder[T] =
    decoder.deriveElementDecoder[T](ElementCodecConfig.default)
}
