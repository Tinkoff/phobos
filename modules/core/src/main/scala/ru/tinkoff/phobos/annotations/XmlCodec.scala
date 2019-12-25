package ru.tinkoff.phobos.annotations

import ru.tinkoff.phobos.{naming => Naming}
import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.reflect.macros.blackbox

@compileTimeOnly("enable macro paradise to expand macro annotations")
class XmlCodec(localName: String, naming: Naming = Naming.asIs) extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro XmlCodecImpl.impl
}

private final class XmlCodecImpl(ctx: blackbox.Context) extends CodecAnnotation(ctx) {
  import c.universe._

  def instances(typ: Tree): Seq[Tree] = {
    val pkg = q"ru.tinkoff.phobos"
    val (localName, naming) = c.prefix.tree match {
      case q"new XmlCodec($localName)"          => (localName, asIsTree)
      case q"new XmlCodec($localName, $naming)" => (localName, naming)
    }

    Seq(
      q"""
          implicit val ${TermName(c.freshName("elementEncoder"))}: $pkg.encoding.ElementEncoder[$typ] =
            $pkg.derivation.semiauto.deriveElementEncoder[$typ]($naming)
       """,
      q"""
          implicit val ${TermName(c.freshName("elementDecoder"))}: $pkg.decoding.ElementDecoder[$typ] =
            $pkg.derivation.semiauto.deriveElementDecoder[$typ]($naming)
       """,
      q"""
          implicit val ${TermName(c.freshName("xmlEncoder"))}: $pkg.encoding.XmlEncoder[$typ] =
            $pkg.encoding.XmlEncoder.fromElementEncoder[$typ]($localName)
       """,
      q"""
          implicit val ${TermName(c.freshName("xmlDecoder"))}: $pkg.decoding.XmlDecoder[$typ] =
            $pkg.decoding.XmlDecoder.fromElementDecoder[$typ]($localName)
       """
    )
  }

}
