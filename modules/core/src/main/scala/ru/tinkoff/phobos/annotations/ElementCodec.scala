package ru.tinkoff.phobos.annotations

import ru.tinkoff.phobos.{naming => Naming}
import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.reflect.macros.blackbox

@compileTimeOnly("enable macro paradise to expand macro annotations")
final class ElementCodec(naming: Naming = Naming.asIs) extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro ElementCodecImpl.impl
}

private final class ElementCodecImpl(ctx: blackbox.Context) extends CodecAnnotation(ctx) {
  import c.universe._

  def instances(typ: Tree): Seq[Tree] = {
    val pkg = q"ru.tinkoff.phobos"

    val naming = c.prefix.tree match {
      case q"new ElementCodec"          => asIsTree
      case q"new ElementCodec($naming)" => naming
    }

    Seq(
      q"""
          implicit val ${TermName(c.freshName("elementEncoder"))}: $pkg.encoding.ElementEncoder[$typ] =
            $pkg.derivation.semiauto.deriveElementEncoder[$typ]($naming)
       """,
      q"""
          implicit val ${TermName(c.freshName("elementDecoder"))}: $pkg.decoding.ElementDecoder[$typ] =
            $pkg.derivation.semiauto.deriveElementDecoder[$typ]($naming)
       """
    )
  }

}
