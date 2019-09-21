package ru.tinkoff.phobos

trait Namespace[T] {
  val getNamespace: String
}

object Namespace {
  def mkInstance[T](uri: String): Namespace[T] = new Namespace[T] {
    val getNamespace: String = uri
  }

  def apply[T](implicit instance: Namespace[T]): Namespace[T] = instance
}