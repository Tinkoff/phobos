package ru.tinkoff.phobos.derivation

/*
 * Copy-pasted from https://github.com/propensive/magnolia
 */

import scala.annotation.compileTimeOnly

@compileTimeOnly("Deferred is used for derivation of recursive typeclasses")
object Deferred { def apply[T](method: String): T = ??? }
