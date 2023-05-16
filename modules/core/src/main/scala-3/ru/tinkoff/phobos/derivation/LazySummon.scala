package ru.tinkoff.phobos.derivation

/** Defining givens of such type in companion objects of ElementEncoder and ElementDecoder allows to summon instances of
  * these typeclasses for every child of a sum type (sealed trait or enum), e.g. like this:
  * {{{
  *   summonAll[Tuple.Map[m.MirroredElemTypes, [t] =>> LazySummon[TC, t]]]
  * }}}
  * while safeguards against automatical derivation for all types without explicit `derives` clause or
  * `deriveElementEncoder`/`deriveElementDecoder` calls.
  */
trait LazySummon[TC[_], A]:
  def instance: TC[A]
