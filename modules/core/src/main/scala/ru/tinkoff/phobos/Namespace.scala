package ru.tinkoff.phobos

/** Typeclass for defining XML namespaces.
  *
  * Example of use:
  *
  * case object nsi { implicit val ns: Namespace[nsi.type] = Namespace.mkInstance("example.com") }
  *
  * See package annotations for more convenient syntax.
  */
trait Namespace[T] {
  val getNamespace: String
}

object Namespace {
  def mkInstance[T](uri: String): Namespace[T] = new Namespace[T] {
    val getNamespace: String = uri
  }

  def apply[T](implicit instance: Namespace[T]): Namespace[T] = instance
}
