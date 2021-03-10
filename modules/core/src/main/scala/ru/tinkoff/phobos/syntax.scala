package ru.tinkoff.phobos
import scala.annotation.StaticAnnotation

/** Syntax annotations for case class params. See ru.tinkoff.derivation.semiato docs for more explanation.
  */
object syntax {

  /** Case class params with @attr annotation are treated as element attributes.
    */
  final class attr() extends StaticAnnotation

  /** Case class param with @text annotation is treated as text inside elements.
    *
    * Only one @text param is allowed per case class.
    */
  final class text() extends StaticAnnotation

  /** Case class param with @default annotation is used for decoding elements
    * which do not fit for other params.
    *
    * Params with this annotation are encoded as regular elements.
    *
    * Only one @default param is allowed per case class.
    */
  final class default() extends StaticAnnotation

  /** Annotation @xmlns adds namespace to case class parameter if implicit Namespace[T] exists.
    *
    * This annotation can be used both for elements and attributes.
    */
  final class xmlns[T](ns: T) extends StaticAnnotation

  /** Allows to rename xml element or attribute name while encoding and decoding
    */
  final class renamed(to: String) extends StaticAnnotation

  /** Allows to define custom type discriminator value for sealed trait instance
    */
  final class discriminator(value: String) extends StaticAnnotation
}
