package ru.tinkoff.phobos.annotations

import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.reflect.macros.blackbox

@compileTimeOnly("enable macro paradise to expand macro annotations")
final class XmlnsDef(uri: String) extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro XmlnsDefImpl.impl
}

private final class XmlnsDefImpl(ctx: blackbox.Context) extends CodecAnnotation(ctx) {
  import c.universe._

  def instances(typ: Tree): Seq[Tree] = {
    val pkg          = q"ru.tinkoff.phobos"
    val instanceName = TermName(c.freshName("namespaceInstance"))
    val uri = c.prefix.tree match {
      case q"new XmlnsDef($uri)" => uri
    }
    Seq(q"implicit val $instanceName: $pkg.Namespace[$typ] = $pkg.Namespace.mkInstance[$typ]($uri)")
  }

}
