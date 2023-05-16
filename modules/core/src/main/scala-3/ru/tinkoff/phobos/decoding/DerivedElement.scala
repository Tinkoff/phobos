package ru.tinkoff.phobos.decoding

import ru.tinkoff.phobos.configured.ElementCodecConfig
import ru.tinkoff.phobos.derivation.decoder
import ru.tinkoff.phobos.derivation.LazySummon
import scala.deriving.Mirror

private[decoding] trait DerivedElement {
  inline def derived[T]: ElementDecoder[T] =
    decoder.deriveElementDecoder[T](ElementCodecConfig.default)

  inline given [T](using mirror: Mirror.Of[T]): LazySummon[ElementDecoder, T] = new:
    def instance = decoder.deriveElementDecoder[T](ElementCodecConfig.default)
}
