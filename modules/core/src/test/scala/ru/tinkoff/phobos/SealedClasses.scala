package ru.tinkoff.phobos

import ru.tinkoff.phobos.annotations.ElementCodec
import ru.tinkoff.phobos.configured.ElementCodecConfig

object SealedClasses {
  @ElementCodec
  sealed trait Foo


  @ElementCodec
  case class Foo1(a: String) extends Foo
  @ElementCodec
  case class Foo2(b: Int) extends Foo
  @ElementCodec
  case class Foo3(c: Double) extends Foo


  @ElementCodec(ElementCodecConfig.default.withDiscriminator("discriminator", None))
  sealed trait Bar

  @ElementCodec
  case class Bar1(a: String) extends Bar
  @ElementCodec
  case class Bar2(b: Int) extends Bar
  @ElementCodec
  case class Bar3(c: Double) extends Bar


  @ElementCodec(ElementCodecConfig.default.withDiscriminator("discriminator", Some("https://tinkoff.ru")))
  sealed trait Baz

  @ElementCodec
  case class Baz1(a: String) extends Baz
  @ElementCodec
  case class Baz2(b: Int) extends Baz
  @ElementCodec
  case class Baz3(c: Double) extends Baz
}
