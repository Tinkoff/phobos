package ru.tinkoff.phobos.derivation

object CallByNeed { def apply[A](a: => A): CallByNeed[A] = new CallByNeed(() => a) }

final class CallByNeed[+A](private[this] var eval: () => A) extends Serializable {
  lazy val value: A = {
    val result = eval()
    eval = null
    result
  }
}
