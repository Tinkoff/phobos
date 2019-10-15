package ru.tinkoff.phobos.derivation

/*
 * Copy-pasted from https://github.com/propensive/magnolia
 */

object CallByNeed { def apply[A](a: => A): CallByNeed[A] = new CallByNeed(() => a) }

final class CallByNeed[+A](private[this] var eval: () => A) extends Serializable {
  lazy val value: A = {
    val result = eval()
    eval = null
    result
  }
}
