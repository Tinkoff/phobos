package ru.tinkoff.phobos.derivation

import scala.annotation.compileTimeOnly

@compileTimeOnly("Deferred is used for derivation of recursive typeclasses")
object Deferred { def apply[T](method: String): T = ??? }