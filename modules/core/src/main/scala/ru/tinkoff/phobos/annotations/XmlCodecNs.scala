package ru.tinkoff.phobos.annotations

import ru.tinkoff.phobos.Namespace

import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.reflect.macros.blackbox

@compileTimeOnly("enable macro paradise to expand macro annotations")
class XmlCodecNs[T: Namespace](localName: String, namespace: T) extends StaticAnnotation {
  def namespaceUri: String = Namespace[T].getNamespace
  def macroTransform(annottees: Any*): Any = macro XmlCodecNsImpl.impl
}

private final class XmlCodecNsImpl(ctx: blackbox.Context) extends CodecAnnotation(ctx) {
  import c.universe._

  def instances(typ: Tree): Seq[Tree] = {
    val pkg = q"ru.tinkoff.phobos"
    val (nsInstance, localName) = c.prefix.tree match {
      case q"new XmlCodecNs($localName, $nsInstance)" => (nsInstance, localName)
      case q"new XmlCodecNs[$_]($localName, $nsInstance)" => (nsInstance, localName)
    }

    Seq(
      q"""
          implicit val ${TermName(c.freshName("elementEncoder"))}: $pkg.encoding.ElementEncoder[$typ] =
            $pkg.derivation.deriveElementEncoder[$typ]
       """,
      q"""
          implicit val ${TermName(c.freshName("elementDecoder"))}: $pkg.decoding.ElementDecoder[$typ] =
            $pkg.derivation.deriveElementDecoder[$typ]
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
