package ru.tinkoff.phobos.derivation

import ru.tinkoff.phobos.decoding.ElementDecoder
import ru.tinkoff.phobos.encoding.ElementEncoder

package object auto {
  inline implicit def deriveExportedEncoder[A]: Exported[ElementEncoder[A]] =
    Exported(semiauto.deriveElementEncoder[A])
  implicit def exportEncoder[A](implicit exported: Exported[ElementEncoder[A]]): ElementEncoder[A] = exported.value

  inline implicit def deriveExportedDecoder[A]: Exported[ElementDecoder[A]] =
    Exported(semiauto.deriveElementDecoder[A])
  implicit def exportDecoder[A](implicit exported: Exported[ElementDecoder[A]]): ElementDecoder[A] = exported.value
}
