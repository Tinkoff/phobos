package ru.tinkoff.phobos.derivation

import ru.tinkoff.phobos.Namespace
import ru.tinkoff.phobos.configured.ElementCodecConfig
import ru.tinkoff.phobos.syntax.*
import ru.tinkoff.phobos.encoding.*
import ru.tinkoff.phobos.derivation.common.*

import scala.annotation.nowarn
import scala.compiletime.*
import scala.quoted.*

@nowarn("msg=Use errorAndAbort")
object encoder {

  inline def deriveElementEncoder[T](
      inline config: ElementCodecConfig,
  ): ElementEncoder[T] =
    ${ deriveElementEncoderImpl('{ config }) }

  inline def deriveXmlEncoder[T](
      inline localName: String,
      inline namespace: Option[String],
      inline preferredNamespacePrefix: Option[String],
      inline config: ElementCodecConfig,
  ): XmlEncoder[T] =
    ${ deriveXmlEncoderImpl('{ localName }, '{ namespace }, '{ preferredNamespacePrefix }, '{ config }) }

  def deriveElementEncoderImpl[T: Type](config: Expr[ElementCodecConfig])(using Quotes): Expr[ElementEncoder[T]] = {
    import quotes.reflect.*

    val tpe        = TypeRepr.of[T]
    val typeSymbol = tpe.typeSymbol
    if (typeSymbol.flags.is(Flags.Case)) {
      deriveProduct(config)
    } else if (typeSymbol.flags.is(Flags.Sealed)) {
      deriveSum(config)
    } else {
      report.throwError(s"${typeSymbol} is not a sum type or product type")
    }
  }

  def deriveXmlEncoderImpl[T: Type](
      localName: Expr[String],
      namespace: Expr[Option[String]],
      preferredNamespacePrefix: Expr[Option[String]],
      config: Expr[ElementCodecConfig],
  )(using Quotes): Expr[XmlEncoder[T]] =
    '{
      XmlEncoder.fromElementEncoder[T]($localName, $namespace, $preferredNamespacePrefix)(${
        deriveElementEncoderImpl(config)
      })
    }

  // PRODUCT

  private def encodeAttributes[T: Type](using Quotes)(
      fields: List[ProductTypeField],
      sw: Expr[PhobosStreamWriter],
      a: Expr[T],
  ): Expr[List[Unit]] = {
    import quotes.reflect.*
    val classTypeRepr = TypeRepr.of[T]
    val classSymbol   = classTypeRepr.typeSymbol
    Expr.ofList(fields.map { field =>
      field.typeRepr.asType match {
        case '[t] =>
          '{
            summonInline[AttributeEncoder[t]].encodeAsAttribute(
              ${ Select(a.asTerm, classSymbol.declaredField(field.localName)).asExprOf[t] },
              $sw,
              ${ field.xmlName },
              ${ field.namespaceUri },
            )
          }
      }
    })
  }

  private def encodeText[T: Type](using Quotes)(
      fields: List[ProductTypeField],
      sw: Expr[PhobosStreamWriter],
      a: Expr[T],
  ): Expr[List[Unit]] = {
    import quotes.reflect.*
    val classTypeRepr = TypeRepr.of[T]
    val classSymbol   = classTypeRepr.typeSymbol

    Expr.ofList(fields.map { field =>
      field.typeRepr.asType match {
        case '[t] =>
          '{
            summonInline[TextEncoder[t]]
              .encodeAsText(${ Select(a.asTerm, classSymbol.declaredField(field.localName)).asExprOf[t] }, $sw)
          }
      }
    })
  }

  private def encodeElements[T: Type](using Quotes)(
      fields: List[ProductTypeField],
      sw: Expr[PhobosStreamWriter],
      a: Expr[T],
  ): Expr[List[Unit]] = {
    import quotes.reflect.*
    val classTypeRepr = TypeRepr.of[T]
    val classSymbol   = classTypeRepr.typeSymbol

    Expr.ofList(fields.map { field =>
      field.typeRepr.asType match {
        case '[t] =>
          '{
            summonInline[ElementEncoder[t]].encodeAsElement(
              ${ Select(a.asTerm, classSymbol.declaredField(field.localName)).asExprOf[t] },
              $sw,
              ${ field.xmlName },
              ${ field.namespaceUri },
            )
          }
      }
    })
  }

  private def deriveProduct[T: Type](config: Expr[ElementCodecConfig])(using Quotes): Expr[ElementEncoder[T]] = {
    import quotes.reflect.*
    val classTypeRepr = TypeRepr.of[T]
    val classSymbol   = classTypeRepr.typeSymbol
    val fields        = extractProductTypeFields[T](config)

    val groups = fields.groupBy(_.category)

    '{
      new ElementEncoder[T] {
        def encodeAsElement(
            a: T,
            sw: PhobosStreamWriter,
            localName: String,
            namespaceUri: Option[String],
            preferredNamespacePrefix: Option[String],
        ): Unit = {
          namespaceUri.fold(sw.writeStartElement(localName))(ns =>
            sw.writeStartElement(preferredNamespacePrefix.orNull, localName, ns),
          )
          $config.scopeDefaultNamespace.foreach { uri =>
            sw.writeAttribute("xmlns", uri)
          }
          $config.defineNamespaces.foreach {
            case (uri, Some(prefix)) =>
              if (sw.getNamespaceContext.getPrefix(uri) == null) sw.writeNamespace(prefix, uri)
            case (uri, None) =>
              if (sw.getNamespaceContext.getPrefix(uri) == null) sw.writeNamespace(uri)
          }

          ${ encodeAttributes[T](groups.getOrElse(FieldCategory.attribute, Nil), 'sw, 'a) }
          ${ encodeText[T](groups.getOrElse(FieldCategory.text, Nil), 'sw, 'a) }
          ${
            encodeElements[T](
              (groups.getOrElse(FieldCategory.element, Nil) ::: groups.getOrElse(FieldCategory.default, Nil)),
              'sw,
              'a,
            )
          }

          sw.writeEndElement()
        }
      }
    }
  }

  // SUM

  private def encodeChild[T: Type](using Quotes)(
      config: Expr[ElementCodecConfig],
      child: SumTypeChild,
      childValue: Expr[T],
      sw: Expr[PhobosStreamWriter],
      localName: Expr[String],
      namespaceUri: Expr[Option[String]],
      preferredNamespacePrefix: Expr[Option[String]],
  ): Expr[Unit] = {
    import quotes.reflect.*

    '{
      val instance = summonInline[ElementEncoder[T]]
      if ($config.useElementNameAsDiscriminator) {
        instance.encodeAsElement(${ childValue }, $sw, ${ child.xmlName }, $namespaceUri, $preferredNamespacePrefix)
      } else {
        $sw.memorizeDiscriminator($config.discriminatorNamespace, $config.discriminatorLocalName, ${ child.xmlName })
        instance.encodeAsElement(${ childValue }, $sw, $localName, $namespaceUri, $preferredNamespacePrefix)
      }
    }
  }

  private def deriveSum[T: Type](config: Expr[ElementCodecConfig])(using Quotes): Expr[ElementEncoder[T]] = {
    import quotes.reflect.*

    '{
      new ElementEncoder[T] {
        def encodeAsElement(
            a: T,
            sw: PhobosStreamWriter,
            localName: String,
            namespaceUri: Option[String],
            preferredNamespacePrefix: Option[String],
        ): Unit = {
          ${
            val alternatives =
              extractSumTypeChildren[T](config).map { child =>
                child.typeRepr.asType match {
                  case '[t] =>
                    val childValueSymbol = Symbol.newBind(Symbol.spliceOwner, "child", Flags.EmptyFlags, TypeRepr.of[t])
                    val encode = encodeChild(
                      config,
                      child,
                      Ref(childValueSymbol).asExprOf[t],
                      'sw,
                      'localName,
                      'namespaceUri,
                      'preferredNamespacePrefix,
                    )
                    CaseDef(Bind(childValueSymbol, Typed(Ref(childValueSymbol), TypeTree.of[t])), None, encode.asTerm)
                }
              }
            Match(
              '{ a }.asTerm,
              // Scala 3.0 reports false positive match may not be exhaustive warning
              if (util.Properties.versionNumberString.startsWith("3.0")) alternatives :+ {
                val symbol = Symbol.newBind(Symbol.spliceOwner, "_", Flags.EmptyFlags, TypeRepr.of[T])
                CaseDef(Bind(symbol, Typed(Ref(symbol), TypeTree.of[T])), None, '{ () }.asTerm)
              }
              else alternatives,
            ).asExprOf[Unit]
          }
        }
      }
    }
  }
}
