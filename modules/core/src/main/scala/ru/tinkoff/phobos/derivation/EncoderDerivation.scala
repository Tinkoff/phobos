package ru.tinkoff.phobos.derivation

import ru.tinkoff.phobos.Namespace
import ru.tinkoff.phobos.configured.ElementCodecConfig
import ru.tinkoff.phobos.derivation.CompileTimeState.{CoproductType, ProductType, Stack}
import ru.tinkoff.phobos.encoding.{AttributeEncoder, ElementEncoder, TextEncoder}

import scala.collection.mutable.ListBuffer
import scala.reflect.macros.blackbox

class EncoderDerivation(ctx: blackbox.Context) extends Derivation(ctx) {

  import c.universe._

  def searchType[T: c.WeakTypeTag]: Type = appliedType(c.typeOf[ElementEncoder[_]], c.weakTypeOf[T])

  def deriveCoproductCodec[T: c.WeakTypeTag](stack: Stack[c.type])(
      config: Expr[ElementCodecConfig],
      subtypes: Iterable[SealedTraitSubtype]
  ): Tree = {
    val assignedName = TermName(c.freshName(s"ElementEncoderTypeclass")).encodedName.toTermName

    val preAssignments     = new ListBuffer[Tree]
    val classType          = c.weakTypeOf[T]
    val scalaPkg           = q"_root_.scala"
    val javaPkg            = q"_root_.java.lang"
    val elementEncoderType = typeOf[ElementEncoder[_]]

    val alternatives = subtypes.map { subtype =>
      val requiredImplicit = appliedType(elementEncoderType, subtype.subtypeType)
      val path             = CoproductType(weakTypeOf[T].toString)
      val frame            = stack.Frame(path, appliedType(elementEncoderType, weakTypeOf[T]), assignedName)
      val derivedImplicit = stack.recurse(frame, requiredImplicit) {
        typeclassTree(stack)(subtype.subtypeType, elementEncoderType)
      }

      val ref      = TermName(c.freshName("paramTypeclass"))
      val assigned = deferredVal(ref, requiredImplicit, derivedImplicit)

      preAssignments.append(assigned)

      cq"""sub: ${subtype.subtypeType.resultType} =>
             sw.memorizeDiscriminator($config.discriminatorNamespace, $config.discriminatorLocalName, ${subtype.constructorName})
             $ref.encodeAsElement(sub, sw, localName, namespaceUri)
      """
    }

    q"""
      ..$preAssignments

      new _root_.ru.tinkoff.phobos.encoding.ElementEncoder[$classType] {
        def encodeAsElement(
          a: $classType,
          sw: _root_.ru.tinkoff.phobos.encoding.PhobosStreamWriter,
          localName: $javaPkg.String,
          namespaceUri: $scalaPkg.Option[$javaPkg.String]
        ): $scalaPkg.Unit = {
          a match {
            case ..$alternatives
          }
        }
      }
    """
  }

  def deriveProductCodec[T: c.WeakTypeTag](stack: Stack[c.type])(params: IndexedSeq[CaseClassParam]): Tree = {
    val assignedName = TermName(c.freshName(s"ElementEncoderTypeclass")).encodedName.toTermName

    val scalaPkg = q"_root_.scala"
    val javaPkg  = q"_root_.java.lang"

    val classType            = c.weakTypeOf[T]
    val attributeEncoderType = typeOf[AttributeEncoder[_]]
    val textEncoderType      = typeOf[TextEncoder[_]]
    val elementEncoderType   = typeOf[ElementEncoder[_]]

    val preAssignments = new ListBuffer[Tree]

    val groups = params.groupBy(_.category)

    val encodeAttributes = groups.getOrElse(ParamCategory.attribute, Nil).map { param =>
      val attributeEncoder = appliedType(attributeEncoderType, param.paramType)
      val attributeEncoderInstance =
        Option(c.inferImplicitValue(attributeEncoder))
          .filter(_.nonEmpty)
          .getOrElse(error(s"Could not find $attributeEncoder for encoding $classType"))

      q"$attributeEncoderInstance.encodeAsAttribute(a.${TermName(param.localName)}, sw, ${param.xmlName}, ${param.namespaceUri})"
    }

    val encodeText = groups.getOrElse(ParamCategory.text, Nil).map { param =>
      val textEncoder = appliedType(textEncoderType, param.paramType)
      val textEncoderInstance =
        Option(c.inferImplicitValue(textEncoder))
          .filter(_.nonEmpty)
          .getOrElse(error(s"Could not find $textEncoder for encoding $classType"))

      q"$textEncoderInstance.encodeAsText(a.${TermName(param.localName)}, sw)"
    }

    val encodeElements = groups.getOrElse(ParamCategory.element, Nil).map { param =>
      val requiredImplicit = appliedType(elementEncoderType, param.paramType)
      val paramName        = param.localName
      val path             = ProductType(paramName, weakTypeOf[T].toString)
      val frame            = stack.Frame(path, appliedType(elementEncoderType, weakTypeOf[T]), assignedName)
      val derivedImplicit = stack.recurse(frame, requiredImplicit) {
        typeclassTree(stack)(param.paramType, elementEncoderType)
      }

      val ref      = TermName(c.freshName("paramTypeclass"))
      val assigned = deferredVal(ref, requiredImplicit, derivedImplicit)

      preAssignments.append(assigned)

      q"$ref.encodeAsElement(a.${TermName(param.localName)}, sw, ${param.xmlName}, ${param.namespaceUri})"
    }

    q"""
     ..$preAssignments

     new _root_.ru.tinkoff.phobos.encoding.ElementEncoder[$classType] {
       def encodeAsElement(
         a: $classType,
         sw: _root_.ru.tinkoff.phobos.encoding.PhobosStreamWriter,
         localName: $javaPkg.String,
         namespaceUri: $scalaPkg.Option[$javaPkg.String]
       ): $scalaPkg.Unit = {
         namespaceUri.fold(sw.writeStartElement(localName))(ns => sw.writeStartElement(ns, localName))

         ..$encodeAttributes
         ..$encodeText
         ..$encodeElements

         sw.writeEndElement()
       }
     }
    """
  }

  def xml[T: c.WeakTypeTag](localName: Tree): Tree =
    xmlConfigured[T](localName, defaultConfig)

  def xmlConfigured[T: c.WeakTypeTag](localName: Tree, config: Expr[ElementCodecConfig]): Tree =
    q"""_root_.ru.tinkoff.phobos.encoding.XmlEncoder.fromElementEncoder[${weakTypeOf[T]}]($localName)(${elementConfigured[
      T](config)})"""

  def xmlNs[T: c.WeakTypeTag, NS: c.WeakTypeTag](localName: Tree, ns: Tree): Tree =
    xmlNsConfigured[T, NS](localName, ns, defaultConfig)

  def xmlNsConfigured[T: c.WeakTypeTag, NS: c.WeakTypeTag](localName: Tree,
                                                           ns: Tree,
                                                           config: Expr[ElementCodecConfig]): Tree = {
    val nsInstance = Option(c.inferImplicitValue(appliedType(weakTypeOf[Namespace[_]], weakTypeOf[NS])))
      .filter(_.nonEmpty)
      .getOrElse(error(s"Could not find Namespace instance for $ns"))
    q"""_root_.ru.tinkoff.phobos.encoding.XmlEncoder.fromElementEncoderNs[${weakTypeOf[T]}, ${weakTypeOf[NS]}]($localName, $ns)(${elementConfigured[
      T](config)}, $nsInstance)"""
  }
}
