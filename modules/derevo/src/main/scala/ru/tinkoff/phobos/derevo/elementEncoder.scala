package ru.tinkoff.phobos.derevo
import org.manatki.derevo.{Derevo, Derivation, delegating}
import ru.tinkoff.phobos.encoding.ElementEncoder

@delegating("ru.tinkoff.phobos.derivation.deriveElementEncoder")
object elementEncoder extends Derivation[ElementEncoder] {
  implicit def instance[T]: ElementEncoder[T] = macro Derevo.delegate[ElementEncoder, T]
}
