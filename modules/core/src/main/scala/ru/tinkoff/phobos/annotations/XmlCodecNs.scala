package ru.tinkoff.phobos.annotations

import ru.tinkoff.phobos.Namespace
import ru.tinkoff.phobos.configured.ElementCodecConfig
import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.reflect.macros.blackbox

@compileTimeOnly("enable macro paradise to expand macro annotations")
class XmlCodecNs[T: Namespace](localName: String, namespace: T, config: ElementCodecConfig = ElementCodecConfig.default)
    extends StaticAnnotation {
  def namespaceUri: String = Namespace[T].getNamespace
  def macroTransform(annottees: Any*): Any = macro XmlCodecNsImpl.impl
}

private final class XmlCodecNsImpl(ctx: blackbox.Context) extends CodecAnnotation(ctx) {
  import c.universe._

  def instances(typ: Tree): Seq[Tree] = {
    val pkg = q"ru.tinkoff.phobos"
    val (nsInstance, localName, config) = c.prefix.tree match {
      case q"new XmlCodecNs($localName, $nsInstance)"              => (nsInstance, localName, defaultConfig.tree)
      case q"new XmlCodecNs($localName, $nsInstance, $config)"     => (nsInstance, localName, config)
      case q"new XmlCodecNs[$_]($localName, $nsInstance, $config)" => (nsInstance, localName, config)
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
            $pkg.encoding.XmlEncoder.fromElementEncoderNs($localName, $nsInstance)
       """,
      q"""
          implicit val ${TermName(c.freshName("xmlDecoder"))}: $pkg.decoding.XmlDecoder[$typ] =
            $pkg.decoding.XmlDecoder.fromElementDecoderNs($localName, $nsInstance)
       """
    )
  }

}
