package ru.tinkoff.phobos
import scala.annotation.StaticAnnotation

object syntax {
  final class attr() extends StaticAnnotation
  final class text() extends StaticAnnotation
  final class xmlns[T](ns: T) extends StaticAnnotation
}
