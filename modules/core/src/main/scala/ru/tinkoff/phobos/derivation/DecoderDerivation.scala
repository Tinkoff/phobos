package ru.tinkoff.phobos.derivation

import ru.tinkoff.phobos.Namespace
import ru.tinkoff.phobos.configured.ElementCodecConfig
import ru.tinkoff.phobos.decoding.{AttributeDecoder, ElementDecoder, TextDecoder}
import ru.tinkoff.phobos.derivation.CompileTimeState.{ProductType, Stack}
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.reflect.macros.blackbox

class DecoderDerivation(ctx: blackbox.Context) extends Derivation(ctx) {
  import c.universe._

  def searchType[T: c.WeakTypeTag]: Type = appliedType(c.typeOf[ElementDecoder[_]], c.weakTypeOf[T])

  def deriveCoproductCodec[T: c.WeakTypeTag](stack: Stack[c.type])(subtypes: Iterable[SealedTraitSubtype]): Tree = q""

  def deriveProductCodec[T: c.WeakTypeTag](stack: Stack[c.type])(params: IndexedSeq[CaseClassParam]): Tree = {

    val derivationPkg = q"_root_.ru.tinkoff.phobos.derivation"
    val decodingPkg   = q"_root_.ru.tinkoff.phobos.decoding"
    val scalaPkg      = q"_root_.scala"
    val javaPkg       = q"_root_.java.lang"

    val decoderStateObj = q"$derivationPkg.DecoderDerivation.DecoderState"

    val classType            = c.weakTypeOf[T]
    val attributeDecoderType = typeOf[AttributeDecoder[_]]
    val textDecoderType      = typeOf[TextDecoder[_]]
    val elementDecoderType   = typeOf[ElementDecoder[_]]

    val decoderName  = TypeName(c.freshName("ElementDecoder"))
    val assignedName = TermName(c.freshName(s"ElementDecoderTypeclass")).encodedName.toTermName

    case class Param(classConstructorParam: Tree,
                     defaultValue: Tree,
                     decoderParamAssignment: Tree,
                     goAssignment: Tree,
                     classConstructionForEnum: Tree,
                     decoderConstructionParam: Tree)

    val allParams: mutable.ListBuffer[Param]       = mutable.ListBuffer.empty
    val decodeAttributes: mutable.ListBuffer[Tree] = mutable.ListBuffer.empty
    val decodeElements: mutable.ListBuffer[Tree]   = mutable.ListBuffer.empty
    val decodeText: mutable.ListBuffer[Tree]       = mutable.ListBuffer.empty
    val elementNames: mutable.ListBuffer[Tree]     = mutable.ListBuffer.empty
    val preAssignments                             = new ListBuffer[Tree]

    params.foreach { param =>
      val tempName   = TermName(c.freshName(param.localName))
      val paramName  = TermName(c.freshName(param.localName))
      val forName    = TermName(c.freshName(param.localName))
      val xmlNameVal = TermName(c.freshName(param.localName))
      elementNames.append(
        q"""val $xmlNameVal = ${param.xmlName}"""
      )
      param.category match {
        case ParamCategory.element =>
          val elementDecoder = appliedType(elementDecoderType, param.paramType)
          val path           = ProductType(param.localName, weakTypeOf[T].toString)
          val frame          = stack.Frame(path, appliedType(elementDecoderType, weakTypeOf[T]), assignedName)
          val derivedImplicit = stack.recurse(frame, elementDecoder) {
            typeclassTree(stack)(param.paramType, elementDecoderType)
          }
          val ref      = TermName(c.freshName("paramTypeclass"))
          val assigned = deferredVal(ref, elementDecoder, derivedImplicit)

          preAssignments.append(assigned)

          allParams.append(
            Param(
              decoderParamAssignment = q"private[this] val $paramName: $derivationPkg.CallByNeed[$elementDecoder]",
              defaultValue = q"$derivationPkg.CallByNeed[$elementDecoder]($ref)",
              goAssignment = q"var $tempName: $elementDecoder = $paramName.value",
              decoderConstructionParam = q"$derivationPkg.CallByNeed[$elementDecoder]($tempName)",
              classConstructionForEnum = fq"$forName <- $tempName.result(cursor.history)",
              classConstructorParam = q"$forName"
            ))

          decodeElements.append(
            cq"""
              `$xmlNameVal`  =>
                $tempName = $tempName.decodeAsElement(cursor, $xmlNameVal, ${param.namespaceUri})
                if ($tempName.isCompleted) {
                  $tempName.result(cursor.history) match {
                    case $scalaPkg.Right(_) => go($decoderStateObj.DecodingSelf)
                    case $scalaPkg.Left(error) => new $decodingPkg.ElementDecoder.FailedDecoder[$classType](error)
                  }
                } else {
                  go($decoderStateObj.DecodingElement($xmlNameVal))
                }
            """
          )

        case ParamCategory.attribute =>
          val attributeDecoder = appliedType(attributeDecoderType, param.paramType)
          val attributeDecoderInstance =
            Option(c.inferImplicitValue(attributeDecoder))
              .filter(_.nonEmpty)
              .getOrElse(error(s"Could not find $attributeDecoder for decoding $classType"))

          allParams.append(
            Param(
              decoderParamAssignment =
                q"private[this] val $paramName: $scalaPkg.Option[$scalaPkg.Either[$decodingPkg.DecodingError, ${param.paramType}]]",
              defaultValue = q"$scalaPkg.None",
              goAssignment = q"""
                  var $tempName: $scalaPkg.Option[$scalaPkg.Either[$decodingPkg.DecodingError, ${param.paramType}]] = $paramName
                """,
              decoderConstructionParam = q"$tempName",
              classConstructionForEnum = fq"$forName <- $tempName.get",
              classConstructorParam = q"$forName"
            ))

          decodeAttributes.append(
            q"""
              $tempName = $scalaPkg.Some($attributeDecoderInstance.decodeAsAttribute(cursor, $xmlNameVal, ${param.namespaceUri}))
            """,
          )

        case ParamCategory.text =>
          val textDecoder = appliedType(textDecoderType, param.paramType)
          val textDecoderInstance =
            Option(c.inferImplicitValue(textDecoder))
              .filter(_.nonEmpty)
              .getOrElse(error(s"Could not find $textDecoder for decoding $classType"))

          allParams.append(
            Param(
              decoderParamAssignment = q"private[this] val $paramName: $textDecoder",
              defaultValue = q"$textDecoderInstance",
              goAssignment = q"var $tempName: $textDecoder = $paramName",
              decoderConstructionParam = q"$tempName",
              classConstructionForEnum = fq"$forName <- $tempName.result(cursor.history)",
              classConstructorParam = q"$forName"
            )
          )
          decodeText.append(q"$tempName = $tempName.decodeAsText(cursor, localName, namespaceUri)")
      }
    }

    val parseTextParam = decodeText.headOption.getOrElse(q"")
    val classConstruction = if (allParams.nonEmpty) {
      q"(for (..${allParams.map(_.classConstructionForEnum)}) yield new $classType(..${allParams.map(_.classConstructorParam)}))"
    } else {
      q"($scalaPkg.Right(new $classType()): $scalaPkg.Either[$decodingPkg.DecodingError, $classType])"
    }

    q"""
      ..$preAssignments
      class $decoderName(
        state: $derivationPkg.DecoderDerivation.DecoderState,
        ..${allParams.map(_.decoderParamAssignment)}
      ) extends $decodingPkg.ElementDecoder[$classType] {
        def decodeAsElement(
          cursor : $decodingPkg.Cursor,
          localName: $javaPkg.String,
          namespaceUri: $scalaPkg.Option[$javaPkg.String],
        ): $decodingPkg.ElementDecoder[$classType] = {
          ..${allParams.map(_.goAssignment)}
          ..$elementNames
          @_root_.scala.annotation.tailrec
          def go(currentState: $derivationPkg.DecoderDerivation.DecoderState): $decodingPkg.ElementDecoder[$classType] = {
            if (cursor.getEventType == _root_.com.fasterxml.aalto.AsyncXMLStreamReader.EVENT_INCOMPLETE) {
              cursor.next()
              new $decoderName(currentState, ..${allParams.map(_.decoderConstructionParam)})
            } else currentState match {
              case $decoderStateObj.New =>
                if (cursor.isStartElement) {
                  $decodingPkg.ElementDecoder.errorIfWrongName[$classType](cursor, localName, namespaceUri) match {
                    case $scalaPkg.Some(error) => error
                    case $scalaPkg.None =>
                      ..$decodeAttributes
                      cursor.next()
                      go($decoderStateObj.DecodingSelf)
                  }
                } else {
                  new $decodingPkg.ElementDecoder.FailedDecoder[$classType](cursor.error("Illegal state: not START_ELEMENT"))
                }

              case $decoderStateObj.DecodingSelf =>
                $parseTextParam
                if (cursor.isStartElement) {
                  cursor.getLocalName match {
                    case ..${decodeElements :+
      cq"""field =>
                        cursor.next()
                        go($decoderStateObj.IgnoringElement(field, 0))
                      """}
                  }
                } else if (cursor.isEndElement) {
                  cursor.getLocalName match {
                    case field if field == localName =>
                      $classConstruction match {
                        case $scalaPkg.Right(result) =>
                          cursor.next()
                          new $decodingPkg.ElementDecoder.ConstDecoder[$classType](result)

                        case $scalaPkg.Left(error) =>
                          new $decodingPkg.ElementDecoder.FailedDecoder[$classType](error)
                      }

                    case _ =>
                      cursor.next()
                      go($decoderStateObj.DecodingSelf)
                  }
                } else {
                  cursor.next()
                  go($decoderStateObj.DecodingSelf)
                }

              case $decoderStateObj.DecodingElement(field) =>
                field match {
                  case ..$decodeElements
                }

              case $decoderStateObj.IgnoringElement(field, depth) =>
                if (cursor.isEndElement && cursor.getLocalName == field) {
                  cursor.next()
                  if (depth == 0) {
                    go($decoderStateObj.DecodingSelf)
                  } else {
                    go($decoderStateObj.IgnoringElement(field, depth - 1))
                  }
                } else if (cursor.isStartElement && cursor.getLocalName == field) {
                  cursor.next()
                  go($decoderStateObj.IgnoringElement(field, depth + 1))
                } else {
                  cursor.next()
                  go(currentState)
                }
            }
          }

          go(state)
        }

        def result(history: $scalaPkg.List[$javaPkg.String]): $scalaPkg.Either[$decodingPkg.DecodingError, $classType] =
          $scalaPkg.Left($decodingPkg.DecodingError("Decoding not complete", history))

        val isCompleted: $scalaPkg.Boolean = false
      }

      new $decoderName($decoderStateObj.New, ..${allParams.map(_.defaultValue)})
    """
  }

  def xml[T: c.WeakTypeTag](localName: Tree): Tree =
    xmlConfigured[T](localName, defaultConfig)

  def xmlConfigured[T: c.WeakTypeTag](localName: Tree, config: Expr[ElementCodecConfig]): Tree =
    q"""_root_.ru.tinkoff.phobos.decoding.XmlDecoder.fromElementDecoder[${weakTypeOf[T]}]($localName)(${elementConfigured[
      T](config)})"""

  def xmlNs[T: c.WeakTypeTag, NS: c.WeakTypeTag](localName: Tree, ns: Tree): Tree =
    xmlNsConfigured[T, NS](localName, ns, defaultConfig)

  def xmlNsConfigured[T: c.WeakTypeTag, NS: c.WeakTypeTag](localName: Tree,
                                                           ns: Tree,
                                                           config: Expr[ElementCodecConfig]): Tree = {
    val nsInstance = Option(c.inferImplicitValue(appliedType(weakTypeOf[Namespace[_]], weakTypeOf[NS])))
      .filter(_.nonEmpty)
      .getOrElse(error(s"Could not find Namespace instance for $ns"))
    q"""_root_.ru.tinkoff.phobos.decoding.XmlDecoder.fromElementDecoderNs[${weakTypeOf[T]}, ${weakTypeOf[NS]}]($localName, $ns)(${elementConfigured[
      T](config)}, $nsInstance)"""
  }
}

object DecoderDerivation {
  sealed trait DecoderState
  object DecoderState {
    case object New                                       extends DecoderState
    case object DecodingSelf                              extends DecoderState
    case class DecodingElement(field: String)             extends DecoderState
    case class IgnoringElement(field: String, depth: Int) extends DecoderState
  }
}
