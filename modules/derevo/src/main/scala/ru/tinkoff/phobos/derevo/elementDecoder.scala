package ru.tinkoff.phobos.derevo
import org.manatki.derevo.{Derevo, Derivation, delegating}
import ru.tinkoff.phobos.decoding.ElementDecoder

@delegating("ru.tinkoff.phobos.derivation.semiauto.deriveElementDecoder")
object elementDecoder extends Derivation[ElementDecoder] {
  implicit def instance[T]: ElementDecoder[T] = macro Derevo.delegate[ElementDecoder, T]
}
