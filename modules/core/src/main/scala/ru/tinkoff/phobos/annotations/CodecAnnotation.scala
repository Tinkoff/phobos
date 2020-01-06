package ru.tinkoff.phobos.annotations

import ru.tinkoff.phobos.configured.ElementCodecConfig
import scala.reflect.macros.blackbox

private[phobos] abstract class CodecAnnotation(val c: blackbox.Context) {

  import c.universe._

  def instances(typ: Tree): Seq[Tree]

  def impl(annottees: c.Tree*): c.Tree = {
    annottees match {
      case Seq(q"$mods object $obj extends { ..$earlydefns } with ..$parents { $self => ..$body }") =>
        q"""
            $mods object $obj extends {..$earlydefns} with ..$parents { $self =>
              ..${instances(tq"$obj.type") ++: body}
            }
          """

      case Seq(cls: ClassDef) =>
        q"""
            $cls
            object ${cls.name.toTermName} {
              ..${instances(tq"${cls.name}")}
            }
          """

      case Seq(cls: ClassDef, q"$mods object $obj extends { ..$earlydefns } with ..$parents { $self => ..$body }") =>
        q"""
            $cls
            $mods object $obj extends {..$earlydefns} with ..$parents { $self =>
              ..${instances(tq"${cls.name}") ++: body}
            }
          """

      case _ =>
        c.abort(c.enclosingPosition, "Namespace must be represented with class or object")
    }
  }

  protected val defaultConfig = reify(ElementCodecConfig.default)
}
