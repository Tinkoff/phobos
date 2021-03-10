package ru.tinkoff.phobos.derivation

import ru.tinkoff.phobos.encoding.ElementEncoder
import ru.tinkoff.phobos.decoding.ElementDecoder

/** Importing contents of this package will provide ElementEncoder / ElementDecoder instances for any case class.
  * It may be useful while researching, however do not use this in production, because automatic derivation is
  * less performant than semiautomatic. It also has some issues when deriving codecs in complicated cases.
  */
package object auto {
  implicit def deriveExportedEncoder[A]: Exported[ElementEncoder[A]] = macro ExportMacro.exportEncoder[A]
  implicit def exportEncoder[A](implicit exported: Exported[ElementEncoder[A]]): ElementEncoder[A] = exported.value

  implicit def deriveExportedDecoder[A]: Exported[ElementDecoder[A]] = macro ExportMacro.exportDecoder[A]
  implicit def exportDecoder[A](implicit exported: Exported[ElementDecoder[A]]): ElementDecoder[A] = exported.value
}
