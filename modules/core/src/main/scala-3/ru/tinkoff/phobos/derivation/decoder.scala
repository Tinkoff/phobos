package ru.tinkoff.phobos.derivation

import com.fasterxml.aalto.AsyncXMLStreamReader
import ru.tinkoff.phobos.Namespace
import ru.tinkoff.phobos.configured.ElementCodecConfig
import ru.tinkoff.phobos.decoding.*
import ru.tinkoff.phobos.decoding.ElementDecoder.{ConstDecoder, FailedDecoder}
import ru.tinkoff.phobos.derivation.common.*
import ru.tinkoff.phobos.derivation.decoder.DecoderState.IgnoringElement
import ru.tinkoff.phobos.syntax.*

import scala.annotation.nowarn
import scala.annotation.tailrec
import scala.collection.mutable
import scala.compiletime.*
import scala.quoted.*

@nowarn("msg=Use errorAndAbort")
@nowarn("msg=Use methodMember")
object decoder {

  inline def deriveElementDecoder[T](
    inline config: ElementCodecConfig
  ): ElementDecoder[T] =
    ${deriveElementDecoderImpl('{config})}

  inline def deriveXmlDecoder[T](
    inline localName: String,
    inline namespace: Option[String],
    inline config: ElementCodecConfig
  ): XmlDecoder[T] =
    ${deriveXmlDecoderImpl('{localName}, '{namespace}, '{config})}

  def deriveElementDecoderImpl[T: Type](config: Expr[ElementCodecConfig])(using Quotes): Expr[ElementDecoder[T]] = {
    import quotes.reflect.*
    val tpe = TypeRepr.of[T]
    val typeSymbol = tpe.typeSymbol
    if (typeSymbol.flags.is(Flags.Case)) {
      deriveProduct[T](config)
    } else if (typeSymbol.flags.is(Flags.Sealed)) {
      deriveSum[T](config)
    } else {
      report.throwError(s"${typeSymbol} is not a sum type or product type")
    }
  }

  def deriveXmlDecoderImpl[T: Type](
    localName: Expr[String],
    namespace: Expr[Option[String]],
    config: Expr[ElementCodecConfig],
  )(using Quotes): Expr[XmlDecoder[T]] =
    '{XmlDecoder.fromElementDecoder[T]($localName, $namespace)(${deriveElementDecoderImpl(config)})}

  // PRODUCT

  sealed trait DecoderState
  object DecoderState {
    case object New                                                                 extends DecoderState
    case object DecodingSelf                                                        extends DecoderState
    case class DecodingElement(name: String)                                        extends DecoderState
    case class IgnoringElement(name: String, namespace: Option[String], depth: Int) extends DecoderState
  }

  private def decodeAttributes(using Quotes)(
    groups: Map[FieldCategory, List[ProductTypeField]],
    c: Expr[Cursor],
    currentFieldStates: Expr[mutable.AnyRefMap[String, Any]],
  ): Expr[List[Unit]] = {
    Expr.ofList(
      groups.getOrElse(FieldCategory.attribute, Nil).map { field =>
        field.typeRepr.asType match {
          case '[t] => '{
            val attribute = summonInline[AttributeDecoder[t]]
              .decodeAsAttribute($c, ${field.xmlName}, ${field.namespaceUri})
            ${currentFieldStates}.update(${Expr(field.localName)}, attribute)
          }
        }
      }
    )
  }

  private def decodeText(using Quotes)(
    groups: Map[FieldCategory, List[ProductTypeField]],
    c: Expr[Cursor],
    currentFieldStates: Expr[mutable.AnyRefMap[String, Any]],
  ): Expr[Unit] = {
    groups.get(FieldCategory.text).flatMap(_.headOption)
      .fold('{()}){ text =>
        text.typeRepr.asType match {
          case '[t] =>
            '{
              val res = $currentFieldStates.getOrElse(${Expr(text.localName)}, summonInline[TextDecoder[t]]).asInstanceOf[TextDecoder[t]].decodeAsText($c)
              $currentFieldStates.update(${Expr(text.localName)}, res)
            }
        }
      }
  }

  // Used twice. Should be used once?
  // Replace `<name> match` with `<paramIdx> switch` ?
  private def decodeElementCases[T: Type](using Quotes)(
    elements: List[ProductTypeField],
    go: Expr[DecoderState => ElementDecoder[T]],
    c: Expr[Cursor],
    currentFieldStates: Expr[mutable.AnyRefMap[String, Any]]
  ): List[quotes.reflect.CaseDef] = {
    import quotes.reflect.*
    elements.map{ element =>
      val symbol = Symbol.newBind(Symbol.spliceOwner, "x", Flags.EmptyFlags, TypeRepr.of[String])
      val eq = symbol.memberMethod("==").head
      CaseDef(Bind(symbol, Typed(Ref(symbol), TypeTree.of[String])), Some(Apply(Select(Ref(symbol), eq), List(element.xmlName.asTerm))),
          (element.typeRepr.asType match {
            case '[t] =>
              '{
                val res = ${currentFieldStates}
                  .getOrElse(${Expr(element.localName)}, summonInline[ElementDecoder[t]])
                  .asInstanceOf[ElementDecoder[t]]
                  .decodeAsElement($c, ${element.xmlName}, ${element.namespaceUri}.orElse($c.getScopeDefaultNamespace))
                ${currentFieldStates}.update(${Expr(element.localName)}, res)
                if (res.isCompleted) {
                  res.result(${element.xmlName} :: $c.history) match {
                    case Right(_) => $go(DecoderState.DecodingSelf)
                    case Left(error) => new ElementDecoder.FailedDecoder[T](error)
                  }
                } else {
                  $go(DecoderState.DecodingElement(${element.xmlName}))
                }
              }
          }).asTerm
        )
      }
    }

  private def decodeStartElement[T: Type](using Quotes)(
    groups: Map[FieldCategory, List[ProductTypeField]],
    go: Expr[DecoderState => ElementDecoder[T]],
    c: Expr[Cursor],
    currentFieldStates: Expr[mutable.AnyRefMap[String, Any]],
  ) = {
    import quotes.reflect.*
    val decodeElements = decodeElementCases[T](groups.getOrElse(FieldCategory.element, Nil), go, c, currentFieldStates)
    val decodeDefault =
      groups.get(FieldCategory.default).flatMap(_.headOption).fold{
        val symbol = Symbol.newBind(Symbol.spliceOwner, "_", Flags.EmptyFlags, TypeRepr.of[String])
        CaseDef(Bind(symbol, Typed(Ref(symbol), TypeTree.of[String])), None, ('{
          val state = DecoderState.IgnoringElement($c.getLocalName, Option($c.getNamespaceURI).filter(_.nonEmpty), 0)
          $c.next()
          $go(state)
        }).asTerm)
      }{ default =>
        val symbol = Symbol.newBind(Symbol.spliceOwner, "_", Flags.EmptyFlags, TypeRepr.of[String])
        CaseDef(Bind(symbol, Typed(Ref(symbol), TypeTree.of[String])), None,
          default.typeRepr.asType match {
            case '[t] =>
              '{
                val name = $c.getLocalName
                val namespace = Option($c.getNamespaceURI)
                val res = $currentFieldStates
                  .getOrElse(${Expr(default.localName)}, summonInline[ElementDecoder[t]])
                  .asInstanceOf[ElementDecoder[t]]
                  .decodeAsElement($c, name, namespace.orElse($c.getScopeDefaultNamespace))
                $currentFieldStates.update(${Expr(default.localName)}, res)
                if (res.isCompleted) {
                  res.result(name :: $c.history) match {
                    case Right(_) => $go(DecoderState.DecodingSelf)
                    case Left(error) => new ElementDecoder.FailedDecoder[T](error)
                  }
                } else {
                  $go(DecoderState.IgnoringElement(name, namespace, 0))
                }
              }.asTerm
          }
        )
      }
    Match('{$c.getLocalName}.asTerm, decodeElements :+ decodeDefault).asExprOf[ElementDecoder[T]]
  }

  private def decodeEndElement[T: Type](using Quotes)(
    fields: List[ProductTypeField],
    go: Expr[DecoderState => ElementDecoder[T]],
    c: Expr[Cursor],
    localName: Expr[String],
    currentFieldStates: Expr[mutable.AnyRefMap[String, Any]],
    config: Expr[ElementCodecConfig],
  ) = {
    import quotes.reflect.*

    '{$c.getLocalName match {
      case name if name == $localName =>
        ${
          fields
            .foldLeft[List[Term] => Expr[Either[DecodingError, T]]](
              terms => '{Right(${Apply(Select(New(TypeTree.of[T]), TypeRepr.of[T].typeSymbol.primaryConstructor), terms).asExprOf[T]})}
            ) { (acc, field) => params =>
              field.typeRepr.asType match {
                case '[t] =>
                  val fSymbol = Symbol.newMethod(
                      Symbol.spliceOwner,
                      "anonfun",
                      MethodType(List(field.localName))(
                      _ => List(TypeRepr.of[t]),
                      _ => TypeRepr.of[Either[DecodingError, T]])
                  )
                  val f = Block(
                    List(DefDef(fSymbol, _.headOption.flatMap(_.headOption).map { param =>
                        acc(param.asExprOf[t].asTerm :: params).asTerm.changeOwner(fSymbol)
                      }
                    )),
                    Closure(Ref(fSymbol), Some(TypeRepr.of[t => Either[DecodingError, T]])),
                  )
                  field.category match {
                    case FieldCategory.element | FieldCategory.default =>
                      '{
                        $currentFieldStates
                          .getOrElse(${Expr(field.localName)}, summonInline[ElementDecoder[t]])
                          .asInstanceOf[ElementDecoder[t]]
                          .result($c.history)
                          .flatMap { ${f.asExprOf[t => Either[DecodingError, T]]} }
                      }
                    case FieldCategory.attribute =>
                      '{
                        $currentFieldStates
                          .getOrElse(
                            ${Expr(field.localName)},
                            Left(DecodingError(s"Attribute '${${field.xmlName}}' is missing or invalid", $c.history))
                          )
                          .asInstanceOf[Either[DecodingError, t]]
                          .flatMap { ${f.asExprOf[t => Either[DecodingError, T]]} }
                      }
                    case FieldCategory.text =>
                      '{
                        $currentFieldStates
                          .getOrElse(
                            ${Expr(field.localName)},
                            summonInline[TextDecoder[t]],
                          )
                          .asInstanceOf[TextDecoder[t]]
                          .result($c.history)
                          .flatMap { ${f.asExprOf[t => Either[DecodingError, T]]} }
                      }
                  }
              }
            }(Nil)
        } match {
          case Right(result) =>
            $c.next()
            $config.scopeDefaultNamespace.foreach(_ => $c.unsetScopeDefaultNamespace())
            new ConstDecoder[T](result)
          case Left(error) =>
            new FailedDecoder[T](error)
        }
      case _ =>
        $c.next()
        $go(DecoderState.DecodingSelf)
    }}
  }

  private def decodingElement[T: Type](using Quotes)(
    groups: Map[FieldCategory, List[ProductTypeField]],
      go: Expr[DecoderState => ElementDecoder[T]],
      c: Expr[Cursor],
      currentFieldStates: Expr[mutable.AnyRefMap[String, Any]],
      name: Expr[String],
  ) = {
    import quotes.reflect.*
    val default = {
      val symbol = Symbol.newBind(Symbol.spliceOwner, "unknown", Flags.EmptyFlags, TypeRepr.of[String])
      CaseDef(Bind(symbol, Typed(Ref(symbol), TypeTree.of[String])), None, '{
        new ElementDecoder.FailedDecoder[T](
          $c.error(
            s"Illegal decoder state: DecodingElement(${${Ref(symbol).asExprOf[String]}}). It's a library bug. Please report it"
          )
        )
      }.asTerm)
    }
    Match(name.asTerm, decodeElementCases[T](groups.getOrElse(FieldCategory.element, Nil), go, c, currentFieldStates) :+ default).asExprOf[ElementDecoder[T]]
  }

  private def ignoringElement[T: Type](using Quotes)(
    groups: Map[FieldCategory, List[ProductTypeField]],
    go: Expr[DecoderState => ElementDecoder[T]],
    c: Expr[Cursor],
    currentFieldStates: Expr[mutable.AnyRefMap[String, Any]],
    state: Expr[IgnoringElement],
  ) = {
    import quotes.reflect.*
    groups.getOrElse(FieldCategory.default, Nil).headOption.fold(
      '{
      if ($c.isEndElement && $c.getLocalName == $state.name && $c.getNamespaceURI == $state.namespace.getOrElse("")) {
        $c.next()
        if ($state.depth == 0) {
          $go(DecoderState.DecodingSelf)
        } else {
          $go($state.copy(depth = $state.depth - 1))
        }
      } else if ($c.isStartElement && $c.getLocalName == $state.name && $c.getNamespaceURI == $state.namespace.getOrElse("")) {
        $c.next()
        $go($state.copy(depth = $state.depth + 1))
      } else {
        $c.next()
        $go($state)
      }}){ default =>
        // Looks similar with code in decodeStartElement
        default.typeRepr.asType match {
          case '[t] =>
           '{
             val res = $currentFieldStates
                .getOrElse(${Expr(default.localName)}, summonInline[ElementDecoder[t]])
                .asInstanceOf[ElementDecoder[t]]
                .decodeAsElement($c, $state.name, $state.namespace.orElse($c.getScopeDefaultNamespace))
             $currentFieldStates.update(${Expr(default.localName)}, res)
             if (res.isCompleted) {
               res.result($state.name :: $c.history) match {
                 case Right(_) => $go(DecoderState.DecodingSelf)
                 case Left(error) => new ElementDecoder.FailedDecoder[T](error)
               }
             } else {
               $go($state)
             }
           }
        }
      }
  }

  private def deriveProduct[T: Type](config: Expr[ElementCodecConfig])(using Quotes): Expr[ElementDecoder[T]] = {
    import quotes.reflect.*
    val classTypeRepr = TypeRepr.of[T]
    val classSymbol = classTypeRepr.typeSymbol
    val fields = extractProductTypeFields[T](config)
    val groups = fields.groupBy(_.category)
    '{
      // Generate case class instead of untyped map?
      class TDecoder(state: DecoderState, fieldStates: Map[String, Any]) extends ElementDecoder[T] {
        def decodeAsElement(c: Cursor, localName: String, namespaceUri: Option[String]): ElementDecoder[T] = {
          val currentFieldStates: mutable.AnyRefMap[String, Any] = mutable.AnyRefMap.from(fieldStates)
          @tailrec
          def go(currentState: DecoderState): ElementDecoder[T] = {
            if (c.getEventType == AsyncXMLStreamReader.EVENT_INCOMPLETE) {
              c.next()
              TDecoder(currentState, Map.from(currentFieldStates))
            } else currentState match {
              case DecoderState.New =>
                if (c.isStartElement) {
                  val newNamespaceUri =
                    if (c.getScopeDefaultNamespace == namespaceUri) $config.scopeDefaultNamespace
                    else $config.scopeDefaultNamespace.orElse(namespaceUri)
                  $config.scopeDefaultNamespace.foreach(c.setScopeDefaultNamespace)
                  ElementDecoder.errorIfWrongName[T](c, localName, newNamespaceUri.orElse(c.getScopeDefaultNamespace)) match {
                    case None =>
                      ${decodeAttributes(groups, 'c, 'currentFieldStates)}
                      c.next()
                      go(DecoderState.DecodingSelf)
                    case Some(error) => error
                  }
                } else {
                  ElementDecoder.FailedDecoder[T](c.error("Illegal state: not START_ELEMENT"))
                }
              case DecoderState.DecodingSelf =>
                ${decodeText(groups, 'c, 'currentFieldStates)}
                if (c.isStartElement) {
                  ${decodeStartElement[T](groups, 'go, 'c, 'currentFieldStates)}
                } else if (c.isEndElement) {
                  ${decodeEndElement[T](fields, 'go, 'c, 'localName, 'currentFieldStates, config)}
                } else {
                  c.next()
                  go(DecoderState.DecodingSelf)
                }
              case DecoderState.DecodingElement(name) =>
                ${decodingElement(groups, 'go, 'c, 'currentFieldStates, 'name)}
              case state: DecoderState.IgnoringElement =>
                ${ignoringElement[T](groups, 'go, 'c, 'currentFieldStates, 'state)}
            }
          }
          go(state)
        }
        def result(history: => List[String]): Either[DecodingError, T] =
          Left(ElementDecoder.decodingNotCompleteError(history))
        val isCompleted: Boolean = false
      }
      TDecoder(DecoderState.New, Map.empty)
    }
  }

  private def deriveSum[T: Type](config: Expr[ElementCodecConfig])(using Quotes): Expr[ElementDecoder[T]] = {
    import quotes.reflect.*
    '{
      new ElementDecoder[T] {
        def decodeAsElement(c: Cursor, localName: String, namespaceUri: Option[String]): ElementDecoder[T] = {
          if (c.getEventType == AsyncXMLStreamReader.EVENT_INCOMPLETE) {
            this
          } else {
            val discriminator = if ($config.useElementNameAsDiscriminator) {
              Right(c.getLocalName)
            } else {
              ElementDecoder.errorIfWrongName[T](c, localName, namespaceUri)
                .map(Left.apply)
                .getOrElse{
                  val discriminatorIdx = c.getAttributeIndex($config.discriminatorNamespace.getOrElse(null), $config.discriminatorLocalName)
                  if (discriminatorIdx > -1) {
                    Right(c.getAttributeValue(discriminatorIdx))
                  } else {
                    Left(
                      new FailedDecoder[T](
                        c.error(s"No type discriminator '${$config.discriminatorNamespace.fold("")(_ + ":")}${$config.discriminatorLocalName}' found")
                      )
                    )
                  }
                }
            }
            discriminator.fold(identity, d => ${Match('d.asTerm, extractSumTypeChildren[T](config).map { child =>
              val symbol = Symbol.newBind(Symbol.spliceOwner, "x", Flags.EmptyFlags, TypeRepr.of[String])
              val eq = symbol.memberMethod("==").head
              CaseDef(Bind(symbol, Typed(Ref(symbol), TypeTree.of[String])), Some(Apply(Select(Ref(symbol), eq), List(child.xmlName.asTerm))), child.typeRepr.asType match {
                  case '[t] => '{
                    summonInline[ElementDecoder[t]]
                      .decodeAsElement(c, c.getLocalName, Option(c.getNamespaceURI).filter(_.nonEmpty).orElse(c.getScopeDefaultNamespace))
                      .map(_.asInstanceOf[T])
                  }.asTerm
                })
              } :+ {
                val symbol = Symbol.newBind(Symbol.spliceOwner, "unknown", Flags.EmptyFlags, TypeRepr.of[String])
                CaseDef(Bind(symbol, Typed(Ref(symbol), TypeTree.of[String])), None,
                  '{new FailedDecoder[T](c.error(s"Unknown type discriminator value: '${${Ref(symbol).asExprOf[String]}}'"))}.asTerm
                )
              }).asExprOf[ElementDecoder[T]]
            })
          }
        }

        val isCompleted: Boolean = false

        def result(history: => List[String]): Either[DecodingError, T] =
          Left(ElementDecoder.decodingNotCompleteError(history))
      }
    }
  }
}
