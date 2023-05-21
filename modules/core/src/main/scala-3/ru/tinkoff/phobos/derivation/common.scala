package ru.tinkoff.phobos.derivation

import ru.tinkoff.phobos.Namespace
import ru.tinkoff.phobos.configured.ElementCodecConfig
import ru.tinkoff.phobos.syntax.*

import scala.quoted.*
import scala.compiletime.*
import scala.annotation.nowarn
import scala.deriving.Mirror
import scala.reflect.TypeTest

@nowarn("msg=Use errorAndAbort")
object common {

  private[derivation] sealed trait FieldCategory

  private[derivation] object FieldCategory {
    case object element   extends FieldCategory
    case object attribute extends FieldCategory
    case object text      extends FieldCategory
    case object default   extends FieldCategory
  }

  private[derivation] final class ProductTypeField(using val quotes: Quotes)(
      val localName: String,
      val xmlName: Expr[String], // Name of element or attribute
      val namespaceUri: Expr[Option[String]],
      val typeRepr: quotes.reflect.TypeRepr,
      val category: FieldCategory,
  )

  private[derivation] final class SumTypeChild[TC[_], Base](
      val xmlName: String,
      val lazyTC: LazySummon[TC, Base],
      val typeTest: TypeTest[Base, ?],
  )

  extension [TC[_], Base](children: List[SumTypeChild[TC, Base]])
    def byInstance[T](i: Base): Option[SumTypeChild[TC, Base]] =
      children.find(_.typeTest.unapply(i).isDefined)

    def byXmlName(n: String): Option[SumTypeChild[TC, Base]] = children.find(_.xmlName == n)

  private[derivation] def extractProductTypeFields[T: Type](
      config: Expr[ElementCodecConfig],
  )(using Quotes): List[ProductTypeField] = {
    import quotes.reflect.*

    val classTypeRepr = TypeRepr.of[T]
    val classSymbol   = classTypeRepr.typeSymbol

    // Extracting first non-type parameter list. Size of this parameter list must be equal to size of .caseFields
    val constructorFields = classSymbol.primaryConstructor.paramSymss.filterNot(_.exists(_.isType)).head
    val fields = classSymbol.caseFields.zip(constructorFields).map { (fieldSymbol, constructorFieldSymbol) =>
      val fieldAnnotations = constructorFieldSymbol.annotations.map(_.asExpr)
      val fieldCategory    = extractFieldCategory(classSymbol, fieldSymbol, fieldAnnotations)
      val fieldXmlName     = extractFieldXmlName(config, classSymbol, fieldSymbol, fieldAnnotations, fieldCategory)
      val fieldNamespace   = extractFeildNamespace(config, classSymbol, fieldSymbol, fieldAnnotations, fieldCategory)
      ProductTypeField(using quotes)(
        fieldSymbol.name,
        fieldXmlName,
        fieldNamespace,
        classTypeRepr.memberType(fieldSymbol),
        fieldCategory,
      )
    }
    val textCount    = fields.count(_.category == FieldCategory.text)
    val defaultCount = fields.count(_.category == FieldCategory.default)
    if (textCount > 1)
      report.throwError(
        s"""
           |Product type cannot have more than one field with @text annotation.
           |Product type '${classSymbol.name}' has $textCount
           |""".stripMargin,
      )
    if (defaultCount > 1)
      report.throwError(
        s"""
           |Product type cannot have more than one field with @default annotation.
           |Product type '${classSymbol.name}' has $defaultCount
           |""".stripMargin,
      )
    fields
  }

  inline def extractSumTypeChild[TC[_], T](
      inline config: ElementCodecConfig,
  )(using m: Mirror.SumOf[T]): List[SumTypeChild[TC, T]] = {
    type Children = m.MirroredElemTypes
    val typeTests = summonAll[Tuple.Map[Children, [t] =>> TypeTest[T, t]]].toList.map(_.asInstanceOf[TypeTest[T, ?]])
    val lazyTCs =
      summonAll[Tuple.Map[Children, [t] =>> LazySummon[TC, t]]].toList.map(_.asInstanceOf[LazySummon[TC, T]])
    val xmlNames = extractSumXmlNames[T](config)

    typeTests.zip(lazyTCs).zip(xmlNames).map { case ((typeTest, lazyTC), xmlName) =>
      new SumTypeChild(xmlName, lazyTC, typeTest)
    }
  }

  private[derivation] inline def extractSumXmlNames[T](inline config: ElementCodecConfig): List[String] =
    ${ extractSumXmlNamesImpl[T]('config) }

  private[derivation] def extractSumXmlNamesImpl[T: Type](
      config: Expr[ElementCodecConfig],
  )(using q: Quotes): Expr[List[String]] = {
    import quotes.reflect.*
    val traitTypeRepr = TypeRepr.of[T]
    val traitSymbol   = traitTypeRepr.typeSymbol

    val names = Varargs(traitSymbol.children.map { childInfoSymbol =>
      extractChildXmlName(using q)(config, traitSymbol, childInfoSymbol)
    })
    '{ List($names: _*) }
  }

  private def extractFieldCategory(using Quotes)(
      classSymbol: quotes.reflect.Symbol,
      fieldSymbol: quotes.reflect.Symbol,
      fieldAnnotations: List[Expr[Any]],
  ): FieldCategory = {
    import quotes.reflect.*
    fieldAnnotations.collect {
      case '{ attr() }    => FieldCategory.attribute
      case '{ text() }    => FieldCategory.text
      case '{ default() } => FieldCategory.default
    } match {
      case Nil            => FieldCategory.element
      case List(category) => category
      case categories =>
        val categoryAnnotations =
          categories.collect {
            case FieldCategory.attribute => "@attr"
            case FieldCategory.text      => "@text"
            case FieldCategory.default   => "@default"
          }.mkString(", ")

        report.throwError(
          s"""
             |Product type field cannot have more than one category annotation (@attr, @text or @default).
             |Field '${fieldSymbol.name}' in product type '${classSymbol.name}' has ${categories.size}: $categoryAnnotations
             |""".stripMargin,
        )
    }
  }

  private def extractFieldXmlName(using Quotes)(
      config: Expr[ElementCodecConfig],
      classSymbol: quotes.reflect.Symbol,
      fieldSymbol: quotes.reflect.Symbol,
      fieldAnnotations: List[Expr[Any]],
      fieldCategory: FieldCategory,
  ): Expr[String] = {
    import quotes.reflect.*
    (fieldAnnotations.collect { case '{ renamed($a) } => a } match {
      case Nil        => None
      case List(name) => Some(name)
      case names =>
        val renamedAnnotations = names.map(name => s"@renamed(${name.asTerm.show})").mkString(", ")
        report.throwError(
          s"""
             |Product type field cannot have more than one @renamed annotation.
             |Field '${fieldSymbol.name}' in product type '${classSymbol.name}' has ${names.size}: $renamedAnnotations
             |""".stripMargin,
        )
    }).getOrElse(fieldCategory match {
      case FieldCategory.element   => '{ ${ config }.transformElementNames(${ Expr(fieldSymbol.name) }) }
      case FieldCategory.attribute => '{ ${ config }.transformAttributeNames(${ Expr(fieldSymbol.name) }) }
      case _                       => Expr(fieldSymbol.name)
    })
  }

  private def extractFeildNamespace(using Quotes)(
      config: Expr[ElementCodecConfig],
      classSymbol: quotes.reflect.Symbol,
      fieldSymbol: quotes.reflect.Symbol,
      fieldAnnotations: List[Expr[Any]],
      fieldCategory: FieldCategory,
  ): Expr[Option[String]] = {
    import quotes.reflect.*
    fieldAnnotations.collect { case '{ xmlns($namespace: b) } =>
      '{ Some(summonInline[Namespace[b]].getNamespace) }
    } match {
      case Nil =>
        fieldCategory match {
          case FieldCategory.element   => '{ ${ config }.elementsDefaultNamespace }
          case FieldCategory.attribute => '{ ${ config }.attributesDefaultNamespace }
          case _                       => '{ None }
        }
      case List(namespace) => namespace
      case namespaces =>
        val xmlnsAnnotations =
          fieldAnnotations.collect { case '{ xmlns($namespace) } =>
            s"@xmlns(${namespace.asTerm.show})"
          }
            .mkString(", ")
        report.throwError(
          s"""
             |Product type field cannot have more than one @xmlns annotation.
             |Field '${fieldSymbol.name}' in product type '${classSymbol.name}' has ${namespaces.size}: $xmlnsAnnotations
             |""".stripMargin,
        )
    }
  }

  private def extractChildXmlName(using Quotes)(
      config: Expr[ElementCodecConfig],
      traitSymbol: quotes.reflect.Symbol,
      childInfoSymbol: quotes.reflect.Symbol,
  ): Expr[String] = {
    import quotes.reflect.*
    childInfoSymbol.annotations.map(_.asExpr).collect { case '{ discriminator($a) } => a } match {
      case Nil        => '{ $config.transformConstructorNames(${ Expr(childInfoSymbol.name) }) }
      case List(name) => name
      case names =>
        val discriminatorAnnotations = names.map(name => s"@discriminator(${name.show})").mkString(", ")
        report.throwError(
          s"""
             |Sum type child cannot have more than one @discriminator annotation.
             |Child '${childInfoSymbol.name}' of sum type '${traitSymbol.name}' has ${names.size}: $discriminatorAnnotations
             |""".stripMargin,
        )
    }
  }

  inline def showType[T <: AnyKind]: String = ${ showTypeMacro[T] }

  private def showTypeMacro[T <: AnyKind: Type](using q: Quotes): Expr[String] =
    import q.reflect.*
    Expr(TypeRepr.of[T].dealias.widen.show)
}
