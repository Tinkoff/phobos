package ru.tinkoff.phobos
import scala.annotation.StaticAnnotation

/**
 * Syntax annotations for case class params. See ru.tinkoff.derivation.semiato docs for more explanation.
 */
object syntax {

  /**
   * Case class params with @attr annotation are treated as element attributes.
   */
  final class attr() extends StaticAnnotation

  /**
   * Case class params with @text annotation are treated as text inside elements.
   */
  final class text() extends StaticAnnotation

  /**
   * Annotation @xmlns adds namespace to case class parameter if implicit Namespace[T] exists.
   */
  final class xmlns[T](ns: T) extends StaticAnnotation

  /**
   * Allows to rename xml tag or attribute name while encoding and decoding
   */
  final class renamed(to: String) extends StaticAnnotation
}
