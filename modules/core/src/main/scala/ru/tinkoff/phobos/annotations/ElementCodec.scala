package ru.tinkoff.phobos.annotations

import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.reflect.macros.blackbox

@compileTimeOnly("enable macro paradise to expand macro annotations")
final class ElementCodec extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro ElementCodecImpl.impl
}

private final class ElementCodecImpl(ctx: blackbox.Context) extends CodecAnnotation(ctx) {
  import c.universe._


  def instances(typ: Tree): Seq[Tree] = {
    val pkg = q"ru.tinkoff.phobos"

    Seq(
      q"""
          implicit val ${TermName(c.freshName("elementEncoder"))}: $pkg.encoding.ElementEncoder[$typ] =
            $pkg.derivation.semiauto.deriveElementEncoder[$typ]
       """,
      q"""
          implicit val ${TermName(c.freshName("elementDecoder"))}: $pkg.decoding.ElementDecoder[$typ] =
            $pkg.derivation.semiauto.deriveElementDecoder[$typ]
       """
    )
  }

}