package ru.tinkoff.phobos.derivation

import ru.tinkoff.phobos.Namespace
import ru.tinkoff.phobos.configured.ElementCodecConfig
import ru.tinkoff.phobos.derivation.CompileTimeState.{ChainedImplicit, Stack}
import ru.tinkoff.phobos.derivation.Derivation.DirectlyReentrantException
import ru.tinkoff.phobos.derivation.auto.Exported
import ru.tinkoff.phobos.syntax.{attr, renamed, text, xmlns}
import scala.reflect.macros.blackbox

private[phobos] abstract class Derivation(val c: blackbox.Context) {

  import c.universe._

  final case class CaseClassParam(localName: String,
                                  xmlName: Tree,
                                  namespaceUri: Tree,
                                  paramType: Type,
                                  category: ParamCategory)

  final case class SealedTraitSubtype(constructorName: String, subtypeType: Type)

  def searchType[T: c.WeakTypeTag]: Type

  def deriveCoproductCodec[T: c.WeakTypeTag](stack: Stack[c.type])(subtypes: Iterable[SealedTraitSubtype]): Tree

  def deriveProductCodec[T: c.WeakTypeTag](stack: Stack[c.type])(params: IndexedSeq[CaseClassParam]): Tree

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
    if (!typeSymbol.isClass) error("Don't know how to work with not classes")
    val classSymbol   = typeSymbol.asClass
    val namespaceType = typeOf[Namespace[_]]
    val attrType      = typeOf[attr]
    val textType      = typeOf[text]
    val xmlnsType     = weakTypeOf[xmlns[_]]
    val renamedType   = typeOf[renamed]

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
          SealedTraitSubtype(symbol.name.decodedName.toString, symbol.asType.toType)
        }
        deriveCoproductCodec(stack)(sealedTraitSubtypes)
      } else if (classSymbol.isCaseClass) {

        def fetchGroup(param: TermSymbol): ParamCategory = {
          param.annotations.foldLeft[List[ParamCategory]](Nil)((acc, annotation) =>
            annotation.tree.tpe match {
              case tpe if tpe == attrType => ParamCategory.attribute :: acc
              case tpe if tpe == textType => ParamCategory.text :: acc
              case _                      => acc
          }) match {
            case List(group) => group
            case Nil         => ParamCategory.element
            case groups =>
              error(s"Parameter ${param.name} must not have multiple xml annotations (${groups.mkString(", ")})")
          }
        }

        def fetchNamespace(param: TermSymbol): Tree =
          param.annotations.collectFirst {
            case annot if annot.tree.tpe <:< xmlnsType =>
              Option(c.inferImplicitValue(appliedType(namespaceType, annot.tree.tpe.typeArgs.head))).map { tree =>
                q"_root_.scala.Some($tree.getNamespace)"
              }.getOrElse(error(s"Namespace typeclass not found for ${annot.tree.tpe.typeArgs.head}"))
          }.getOrElse(q"_root_.scala.None")

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

        val params = caseParams.zip(annotations).map {
          case (paramType, param) =>
            fetchNamespace(param)
            val namespace = fetchNamespace(param)
            val group     = fetchGroup(param)
            val localName = param.name.decodedName.toString
            val xmlName: Tree = param.annotations.collectFirst {
              case annotation if annotation.tree.tpe =:= renamedType =>
                annotation.tree.children.tail.collectFirst {
                  case t @ Literal(Constant(_: String)) => t
                }.getOrElse {
                  error("@renamed is only allowed to be used with string literals")
                }
            } getOrElse {
              val localNameTree = q"""$localName"""
              group match {
                case ParamCategory.attribute =>
                  q"""$config.transformAttributeNames($localNameTree)"""
                case ParamCategory.element =>
                  q"""$config.transformElementNames($localNameTree)"""
                case _ => localNameTree
              }
            }
            CaseClassParam(localName, xmlName, namespace, paramType, group)
        }

        val attributeParamsNumber = params.count(_.category == ParamCategory.attribute)
        val regularParamsNumber   = params.count(_.category == ParamCategory.element)
        val textParamsNumber      = params.count(_.category == ParamCategory.text)

        (attributeParamsNumber, regularParamsNumber, textParamsNumber) match {
          case (_, _, l) if l > 1 => error(s"Multiple @text parameters in one class")
          case _                  => deriveProductCodec(stack)(params)
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
