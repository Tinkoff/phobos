package ru.tinkoff.phobos

import ru.tinkoff.phobos.annotations.ElementCodec
import ru.tinkoff.phobos.encoding.ElementEncoder

object SealedClasses {
//  @ElementCodec
  sealed trait Foo

  object Foo {
    implicit val elementEncoder: ElementEncoder[Foo] = ru.tinkoff.phobos.derivation.semiauto.deriveElementEncoder[Foo]
  }

  @ElementCodec
  case class Foo1(a: String) extends Foo
  @ElementCodec
  case class Foo2(b: Int) extends Foo
  @ElementCodec
  case class Foo3(c: Char) extends Foo
}
