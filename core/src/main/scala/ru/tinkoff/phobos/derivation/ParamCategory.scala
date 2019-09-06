package ru.tinkoff.phobos.derivation

sealed trait ParamCategory

object ParamCategory {
  case object element extends ParamCategory
  case object attribute extends ParamCategory
  case object text extends ParamCategory
}
