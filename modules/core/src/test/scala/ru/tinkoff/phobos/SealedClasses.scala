package ru.tinkoff.phobos

import ru.tinkoff.phobos.annotations.ElementCodec
import ru.tinkoff.phobos.configured.ElementCodecConfig
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
  case class Foo3(c: Double) extends Foo


  sealed trait Bar

  object Bar {
    implicit val elementEncoder: ElementEncoder[Bar] = ru.tinkoff.phobos.derivation.semiauto.deriveElementEncoderConfigured[Bar](ElementCodecConfig.default.withDiscriminator("discriminator", None))
  }

  @ElementCodec
  case class Bar1(a: String) extends Bar
  @ElementCodec
  case class Bar2(b: Int) extends Bar
  @ElementCodec
  case class Bar3(c: Double) extends Bar


  sealed trait Baz

  object Baz {
    implicit val elementEncoder: ElementEncoder[Baz] = ru.tinkoff.phobos.derivation.semiauto.deriveElementEncoderConfigured[Baz](ElementCodecConfig.default.withDiscriminator("discriminator", Some("https://tinkoff.ru")))
  }

  @ElementCodec
  case class Baz1(a: String) extends Baz
  @ElementCodec
  case class Baz2(b: Int) extends Baz
  @ElementCodec
  case class Baz3(c: Double) extends Baz
}
