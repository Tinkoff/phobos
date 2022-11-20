package ru.tinkoff.phobos

/** Typeclass for defining XML namespaces.
  *
  * Usage example:
  *
  * case object nsi { implicit val ns: Namespace[nsi.type] = Namespace.mkInstance("example.com") }
  *
  * Value getPreferredPrefix is an optional preferred prefix to associate with this namespace.
  * Prefix MUST contain only letters, digits and '_', '-' characters, otherwise encoding may fail with an error.
  * If preferred prefix is declared via mkInstance, illegal characters are filtered out.
  *
  * If there are several prefixes for one namespace URI, namespaces may be declared twice.
  *
  * Note: Two namespaces with different prefixes and equal URIs are equal, so users may not declare preferred prefixes
  *       and rely on automatically generated ones.
  *
  *       For example, these two documents are equal, because namespace URIs are equal:
  *         <xmpl:foo xmlns:xmpl="example.com">value</xmpl:foo>
  *         <ans1:foo xmlns:ans1="example.com">value</ans1:foo>
  *
  * See package 'annotations' for more convenient syntax.
  */
trait Namespace[T] {
  val getNamespace: String
  val getPreferredPrefix: Option[String]
}

object Namespace {
  def mkInstance[T](uri: String, preferredPrefix: Option[String] = None): Namespace[T] = new Namespace[T] {
    val getNamespace: String = uri
    val getPreferredPrefix: Option[String] =
      preferredPrefix.map(_.filter(c => c.isLetter || c.isDigit || c == '_' || c == '-'))
  }

  def apply[T](implicit instance: Namespace[T]): Namespace[T] = instance
}
