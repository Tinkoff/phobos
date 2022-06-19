package ru.tinkoff.phobos.annotations

import ru.tinkoff.phobos.configured.ElementCodecConfig

import scala.annotation.nowarn
import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.reflect.macros.blackbox

@compileTimeOnly("enable macro paradise to expand macro annotations")
class XmlCodec(localName: String, config: ElementCodecConfig = ElementCodecConfig.default) extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro XmlCodecImpl.impl
}

private final class XmlCodecImpl(ctx: blackbox.Context) extends CodecAnnotation(ctx) {
  import c.universe._

  def instances(typ: Tree): Seq[Tree] = {
    val pkg = q"ru.tinkoff.phobos"
    val (localName, config) = (c.prefix.tree: @nowarn("msg=not.*?exhaustive")) match {
      case q"new XmlCodec($localName)"          => (localName, defaultConfig.tree)
      case q"new XmlCodec($localName, $config)" => (localName, config)
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
      q"""
          implicit val ${TermName(c.freshName("xmlEncoder"))}: $pkg.encoding.XmlEncoder[$typ] =
            $pkg.encoding.XmlEncoder.fromElementEncoder[$typ]($localName)
       """,
      q"""
          implicit val ${TermName(c.freshName("xmlDecoder"))}: $pkg.decoding.XmlDecoder[$typ] =
            $pkg.decoding.XmlDecoder.fromElementDecoder[$typ]($localName)
       """,
    )
  }

}
