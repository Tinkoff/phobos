package ru.tinkoff.phobos.annotations

import ru.tinkoff.phobos.configured.ElementCodecConfig
import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.reflect.macros.blackbox

@compileTimeOnly("enable macro paradise to expand macro annotations")
final class ElementCodec(config: ElementCodecConfig = ElementCodecConfig.default) extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro ElementCodecImpl.impl
}

private final class ElementCodecImpl(ctx: blackbox.Context) extends CodecAnnotation(ctx) {
  import c.universe._

  def instances(typ: Tree): Seq[Tree] = {
    val pkg = q"ru.tinkoff.phobos"

    val config = c.prefix.tree match {
      case q"new ElementCodec"          => defaultConfig.tree
      case q"new ElementCodec($config)" => config
    }

    Seq(
      q"""
          implicit val ${TermName(c.freshName("elementEncoder"))}: $pkg.encoding.ElementEncoder[$typ] =
            $pkg.derivation.semiauto.deriveElementEncoderConfigured[$typ]($config)
       """,
      q"""
          implicit val ${TermName(c.freshName("elementDecoder"))}: $pkg.decoding.ElementDecoder[$typ] =
            $pkg.derivation.semiauto.deriveElementDecoderConfigured[$typ]($config)
       """,
    )
  }

}
