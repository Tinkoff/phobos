package ru.tinkoff.phobos.derivation

import ru.tinkoff.phobos.Namespace
import ru.tinkoff.phobos.configured.ElementCodecConfig
import ru.tinkoff.phobos.derivation.CompileTimeState.{ChainedImplicit, Stack}
import ru.tinkoff.phobos.derivation.Derivation.DirectlyReentrantException
import ru.tinkoff.phobos.syntax.{xmlns, default, attr, renamed, text, discriminator}

import scala.annotation.nowarn
import scala.reflect.macros.blackbox

private[phobos] abstract class Derivation(val c: blackbox.Context) {

  import c.universe._

  @nowarn("msg=outer reference.*cannot be checked")
  final case class CaseClassParam(
      localName: String,
      xmlName: Tree,
      namespaceUri: Tree,
      preferredNamespacePrefix: Tree,
      paramType: Type,
      category: ParamCategory,
  )
  @nowarn("msg=outer reference.*cannot be checked")
  final case class SealedTraitSubtype(
      constructorName: Tree,
      subtypeType: Type,
  )

  def searchType[T: c.WeakTypeTag]: Type

  def deriveCoproductCodec[T: c.WeakTypeTag](stack: Stack[c.type])(
      config: Expr[ElementCodecConfig],
      subtypes: Iterable[SealedTraitSubtype],
  ): Tree

  def deriveProductCodec[T: c.WeakTypeTag](
      stack: Stack[c.type],
  )(config: Expr[ElementCodecConfig], params: IndexedSeq[CaseClassParam]): Tree

  def error(msg: String): Nothing = c.abort(c.enclosingPosition, msg)

  def deferredVal(name: TermName, tpe: Type, rhs: Tree): Tree = {
    q"lazy val $name: $tpe = $rhs"
  }

  def exportedTypecclass(searchType: Type): Option[Tree] =
    Option(c.inferImplicitValue(appliedType(typeOf[Exported[_]], searchType)))
      .filterNot(_.isEmpty)
      .map(exported => q"$exported.value")

  def typeclassTree(stack: Stack[c.type])(genericType: Type, typeConstructor: Type): Tree = {
    val prefixType   = c.prefix.tree.tpe
    val prefixObject = prefixType.typeSymbol
    val prefixName   = prefixObject.name.decodedName

    val searchType = appliedType(typeConstructor, genericType)
    val deferredRef = for (methodName <- stack find searchType) yield {
      val methodAsString = methodName.decodedName.toString
      q"_root_.ru.tinkoff.phobos.derivation.Deferred.apply[$searchType]($methodAsString)"
    }

    deferredRef.getOrElse {
      val path  = ChainedImplicit(s"$prefixName.Typeclass", genericType.toString)
      val frame = stack.Frame(path, searchType, termNames.EMPTY)
      stack.recurse(frame, searchType) {
        Option(c.inferImplicitValue(searchType))
          .filterNot(_.isEmpty)
          .orElse(exportedTypecclass(searchType))
          .getOrElse {
            val missingType   = stack.top.fold(searchType)(_.searchType)
            val typeClassName = s"${missingType.typeSymbol.name.decodedName}"
            val genericType   = missingType.typeArgs.head
            val trace         = stack.trace.mkString("    in ", "\n    in ", "\n")
            error(s"Could not find $typeClassName for type $genericType\n$trace")
          }
      }
    }
  }

  def element[T: c.WeakTypeTag]: Tree = elementConfigured[T](defaultConfig)

  def elementConfigured[T: c.WeakTypeTag](config: Expr[ElementCodecConfig]): Tree = Stack.withContext(c) { stack =>
    val classType  = weakTypeOf[T]
    val typeSymbol = classType.typeSymbol
    if (!typeSymbol.isClass) error(s"Don't know how to work with not classes ($typeSymbol)")
    val classSymbol       = typeSymbol.asClass
    val namespaceType     = typeOf[Namespace[_]]
    val attrType          = typeOf[attr]
    val textType          = typeOf[text]
    val defaultType       = typeOf[default]
    val xmlnsType         = weakTypeOf[xmlns[_]]
    val renamedType       = typeOf[renamed]
    val discriminatorType = typeOf[discriminator]

    val expandDeferred = new Transformer {
      override def transform(tree: Tree) = tree match {
        case q"_root_.ru.tinkoff.phobos.derivation.Deferred.apply[$_](${Literal(Constant(method: String))})" =>
          q"${TermName(method)}"
        case _ =>
          super.transform(tree)
      }
    }

    def inferCodec: Tree = {
      if (classSymbol.isSealed) {
        val sealedTraitSubtypes = classType.typeSymbol.asClass.knownDirectSubclasses.map { symbol =>
          val constructorName = q"""$config.transformConstructorNames(`${symbol.name.decodedName.toString}`)"""
          val discriminatorValue = symbol.annotations.collectFirst {
            case annot if annot.tree.tpe =:= discriminatorType =>
              annot.tree.children.tail.collectFirst { case t @ Literal(Constant(_: String)) =>
                t
              }.getOrElse {
                error("@discriminator is only allowed to be used with string literals")
              }
          }.getOrElse(constructorName)
          val subType     = symbol.asType.toType
          val typeArgs    = subType.baseType(classType.typeSymbol).typeArgs
          val mapping     = typeArgs.map(_.typeSymbol).zip(classType.typeArgs).toMap
          val newTypeArgs = symbol.asType.typeParams.map(mapping.withDefault(_.asType.toType))
          val applied     = appliedType(subType.typeConstructor, newTypeArgs)
          SealedTraitSubtype(discriminatorValue, applied)
        }
        deriveCoproductCodec(stack)(config, sealedTraitSubtypes)
      } else if (classSymbol.isCaseClass) {

        def fetchCategory(param: TermSymbol): ParamCategory = {
          param.annotations.foldLeft[List[ParamCategory]](Nil)((acc, annotation) =>
            annotation.tree.tpe match {
              case tpe if tpe == attrType    => ParamCategory.attribute :: acc
              case tpe if tpe == textType    => ParamCategory.text :: acc
              case tpe if tpe == defaultType => ParamCategory.default :: acc
              case _                         => acc
            },
          ) match {
            case List(category) => category
            case Nil            => ParamCategory.element
            case categories =>
              error(s"Parameter ${param.name} must not have multiple xml annotations (${categories.mkString(", ")})")
          }
        }

        def fetchNamespace(param: TermSymbol, paramCategory: ParamCategory): (Tree, Tree) =
          param.annotations.collectFirst {
            case annot if annot.tree.tpe <:< xmlnsType =>
              Option(c.inferImplicitValue(appliedType(namespaceType, annot.tree.tpe.typeArgs.head))).map { tree =>
                (q"_root_.scala.Some($tree.getNamespace)", q"$tree.getPreferredPrefix")
              }.getOrElse(error(s"Namespace typeclass not found for ${annot.tree.tpe.typeArgs.head}"))
          }.getOrElse(paramCategory match {
            case ParamCategory.element   => (q"""$config.elementsDefaultNamespace""", q"_root_.scala.None")
            case ParamCategory.attribute => (q"""$config.attributesDefaultNamespace""", q"_root_.scala.None")
            case _                       => (q"_root_.scala.None", q"_root_.scala.None")
          })

        val repeatedParamClass = definitions.RepeatedParamClass
        val scalaSeqType       = typeOf[Seq[_]].typeConstructor

        val caseParams = classType.decls.collect {
          case m: MethodSymbol if m.isCaseAccessor =>
            m.asMethod
        }.map { param =>
          val paramTypeSubstituted = param.typeSignatureIn(classType).resultType

          val paramType = paramTypeSubstituted match {
            case TypeRef(_, `repeatedParamClass`, typeArgs) =>
              appliedType(scalaSeqType, typeArgs)
            case tpe =>
              tpe
          }
          paramType
        }.toIndexedSeq

        val annotations = classSymbol.primaryConstructor.asMethod.typeSignature.paramLists.headOption
          .map(_.map(_.asTerm))
          .toList
          .flatten

        val params = caseParams.zip(annotations).map { case (paramType, param) =>
          val category                              = fetchCategory(param)
          val (namespace, preferredNamespacePrefix) = fetchNamespace(param, category)
          val localName                             = param.name.decodedName.toString
          val xmlName: Tree = param.annotations.collectFirst {
            case annotation if annotation.tree.tpe =:= renamedType =>
              annotation.tree.children.tail.collectFirst { case t @ Literal(Constant(_: String)) =>
                t
              }.getOrElse {
                error("@renamed is only allowed to be used with string literals")
              }
          } getOrElse {
            val localNameTree = q"""$localName"""
            category match {
              case ParamCategory.attribute =>
                q"""$config.transformAttributeNames($localNameTree)"""
              case ParamCategory.element =>
                q"""$config.transformElementNames($localNameTree)"""
              case _ => localNameTree
            }
          }
          CaseClassParam(localName, xmlName, namespace, preferredNamespacePrefix, paramType, category)
        }

        val attributeParamsNumber = params.count(_.category == ParamCategory.attribute)
        val regularParamsNumber   = params.count(_.category == ParamCategory.element)
        val textParamsNumber      = params.count(_.category == ParamCategory.text)
        val defaultParamsNumber   = params.count(_.category == ParamCategory.default)

        (attributeParamsNumber, regularParamsNumber, textParamsNumber, defaultParamsNumber) match {
          case (_, _, t, _) if t > 1 => error("Multiple @text parameters are not allowed")
          case (_, _, _, d) if d > 1 => error("Mutiple @default parameters are not allowed")
          case _                     => deriveProductCodec(stack)(config, params)
        }
      } else error(s"$classSymbol is not case class or sealed trait")
    }

    val directlyReentrant = stack.top.exists(_.searchType =:= searchType)
    if (directlyReentrant) throw DirectlyReentrantException()

    val result = stack
      .find(searchType)
      .map(enclosingRef => q"_root_.ru.tinkoff.phobos.derivation.Deferred[$searchType](${enclosingRef.toString})")
      .getOrElse(inferCodec)

    if (stack.nonEmpty) result
    else c.untypecheck(expandDeferred.transform(result))
  }

  protected val defaultConfig: Expr[ElementCodecConfig] = reify(ElementCodecConfig.default)
}

object Derivation {
  final case class DirectlyReentrantException() extends Exception("attempt to recurse directly")
}
