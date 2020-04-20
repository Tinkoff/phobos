package ru.tinkoff.phobos.ast.impl

import cats.Eq
import ru.tinkoff.phobos.ast.XmlEntry

trait CatsInstances {
  implicit val eqForAst: Eq[XmlEntry] = Eq.fromUniversalEquals[XmlEntry]
}
