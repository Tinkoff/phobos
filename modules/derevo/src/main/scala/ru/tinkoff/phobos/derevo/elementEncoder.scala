package ru.tinkoff.phobos.derevo

import derevo.{Derevo, Derivation, delegating}
import ru.tinkoff.phobos.encoding.ElementEncoder

@delegating("ru.tinkoff.phobos.derivation.semiauto.deriveElementEncoder")
object elementEncoder extends Derivation[ElementEncoder] {
  implicit def instance[T]: ElementEncoder[T] = macro Derevo.delegate[ElementEncoder, T]
}
