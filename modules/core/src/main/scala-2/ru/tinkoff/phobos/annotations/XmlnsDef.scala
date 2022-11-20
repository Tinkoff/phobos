package ru.tinkoff.phobos.annotations

import scala.annotation.nowarn
import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.reflect.macros.blackbox

@compileTimeOnly("enable macro paradise to expand macro annotations")
final class XmlnsDef(uri: String, useNameAsPrefix: Boolean = false) extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro XmlnsDefImpl.impl
}

private final class XmlnsDefImpl(ctx: blackbox.Context) extends CodecAnnotation(ctx) {
  import c.universe._

  @nowarn("msg=not.*?exhaustive")
  def instances(typ: Tree): Seq[Tree] = {
    val pkg          = q"ru.tinkoff.phobos"
    val instanceName = TermName(c.freshName("namespaceInstance"))
    val (uri, useNameAsPrefix) = c.prefix.tree match {
      case q"new XmlnsDef($uri)"                   => (uri, q"false")
      case q"new XmlnsDef($uri, $useNameAsPrefix)" => (uri, useNameAsPrefix)
      case _ =>
        c.abort(c.enclosingPosition, "@XmlnsDef must have exactly one or two parameters")
    }
    val prefix =
      typ match {
        case tq"$name.type" => name.toString
        case tq"$name"      => name.toString
        case _ =>
          c.abort(c.enclosingPosition, "Namespace must be represented with class or object")
      }
    Seq(
      q"""implicit val $instanceName: $pkg.Namespace[$typ] =
         if ($useNameAsPrefix) {
           $pkg.Namespace.mkInstance[$typ]($uri, _root_.scala.Some($prefix))
         } else {
           $pkg.Namespace.mkInstance[$typ]($uri)
         }""",
    )
  }

}
