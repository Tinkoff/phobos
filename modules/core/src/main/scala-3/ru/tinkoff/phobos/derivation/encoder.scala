package ru.tinkoff.phobos.derivation

import ru.tinkoff.phobos.Namespace
import ru.tinkoff.phobos.configured.ElementCodecConfig
import ru.tinkoff.phobos.syntax.*
import ru.tinkoff.phobos.encoding.*
import ru.tinkoff.phobos.derivation.common.*

import scala.annotation.nowarn
import scala.compiletime.*
import scala.quoted.*
import scala.deriving.Mirror

@nowarn("msg=Use errorAndAbort")
object encoder {

  inline def deriveElementEncoder[T](
      inline config: ElementCodecConfig,
  ): ElementEncoder[T] =
    summonFrom {
      case _: Mirror.ProductOf[T] => deriveProduct(config)
      case _: Mirror.SumOf[T] =>
        val childInfos = extractSumTypeChild[ElementEncoder, T](config)
        deriveSum(config, childInfos)
      case _ => error(s"${showType[T]} is not a sum type or product type")
    }

  inline def deriveXmlEncoder[T](
      inline localName: String,
      inline namespace: Option[String],
      inline preferredNamespacePrefix: Option[String],
      inline config: ElementCodecConfig,
  ): XmlEncoder[T] =
    XmlEncoder.fromElementEncoder[T](localName, namespace, preferredNamespacePrefix)(deriveElementEncoder(config))

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

  inline def deriveProduct[T](inline config: ElementCodecConfig): ElementEncoder[T] =
    ${ deriveProductImpl[T]('config) }

  private def deriveProductImpl[T: Type](config: Expr[ElementCodecConfig])(using Quotes): Expr[ElementEncoder[T]] = {
    val fields = extractProductTypeFields[T](config)
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

  inline def deriveSum[T](
      inline config: ElementCodecConfig,
      inline childInfos: List[SumTypeChild[ElementEncoder, T]],
  ): ElementEncoder[T] = {
    new ElementEncoder[T] {
      def encodeAsElement(
          t: T,
          sw: PhobosStreamWriter,
          localName: String,
          namespaceUri: Option[String],
          preferredNamespacePrefix: Option[String],
      ): Unit = {
        val childInfo = childInfos
          .byInstance(t)
          .getOrElse(throw EncodingError(s"Looks like an error in derivation: no TypeTest was positive for $t"))
        val discr =
          if (config.useElementNameAsDiscriminator) childInfo.xmlName
          else {
            sw.memorizeDiscriminator(
              config.discriminatorNamespace,
              config.discriminatorLocalName,
              childInfo.xmlName,
            )
            localName
          }

        childInfo.lazyTC.instance
          .asInstanceOf[ElementEncoder[T]]
          .encodeAsElement(t, sw, discr, namespaceUri, preferredNamespacePrefix)
      }
    }
  }
}
