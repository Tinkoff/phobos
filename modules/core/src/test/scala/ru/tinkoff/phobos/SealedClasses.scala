package ru.tinkoff.phobos

import ru.tinkoff.phobos.configured.ElementCodecConfig
import ru.tinkoff.phobos.configured.naming._
import ru.tinkoff.phobos.decoding.ElementDecoder
import ru.tinkoff.phobos.derivation.semiauto._
import ru.tinkoff.phobos.encoding.ElementEncoder
import ru.tinkoff.phobos.syntax.discriminator

object SealedClasses {
  sealed trait Foo

  case class Foo1(a: String) extends Foo
  case class Foo2(b: Int)    extends Foo
  case class Foo3(c: Double) extends Foo
  case class Yoba(foo1: Foo1)

  object Foo {
    implicit val foo1Encoder: ElementEncoder[Foo1] = deriveElementEncoder
    implicit val foo2Encoder: ElementEncoder[Foo2] = deriveElementEncoder
    implicit val foo3Encoder: ElementEncoder[Foo3] = deriveElementEncoder
    implicit val yobaEncoder: ElementEncoder[Yoba] = deriveElementEncoder
    implicit val fooEncoder: ElementEncoder[Foo]   = deriveElementEncoder
    implicit val foo1Decoder: ElementDecoder[Foo1] = deriveElementDecoder
    implicit val foo2Decoder: ElementDecoder[Foo2] = deriveElementDecoder
    implicit val foo3Decoder: ElementDecoder[Foo3] = deriveElementDecoder
    implicit val fooDecoder: ElementDecoder[Foo]   = deriveElementDecoder
  }

  sealed trait Bar

  case class Bar1(a: String) extends Bar
  case class Bar2(b: Int)    extends Bar
  case class Bar3(c: Double) extends Bar

  object Bar {
    val config                                     = ElementCodecConfig.default.withDiscriminator("discriminator", None)
    implicit val bar1Encoder: ElementEncoder[Bar1] = deriveElementEncoder
    implicit val bar2Encoder: ElementEncoder[Bar2] = deriveElementEncoder
    implicit val bar3Encoder: ElementEncoder[Bar3] = deriveElementEncoder
    implicit val barEncoder: ElementEncoder[Bar]   = deriveElementEncoderConfigured(config)
    implicit val bar1Decoder: ElementDecoder[Bar1] = deriveElementDecoder
    implicit val bar2Decoder: ElementDecoder[Bar2] = deriveElementDecoder
    implicit val bar3Decoder: ElementDecoder[Bar3] = deriveElementDecoder
    implicit val barDecoder: ElementDecoder[Bar]   = deriveElementDecoderConfigured(config)
  }

  sealed trait Baz

  case class Baz1(a: String) extends Baz
  case class Baz2(b: Int)    extends Baz
  case class Baz3(c: Double) extends Baz

  object Baz {
    val config                                     = ElementCodecConfig.default.withDiscriminator("discriminator", Some("https://tinkoff.ru"))
    implicit val baz1Encoder: ElementEncoder[Baz1] = deriveElementEncoder
    implicit val baz2Encoder: ElementEncoder[Baz2] = deriveElementEncoder
    implicit val baz3Encoder: ElementEncoder[Baz3] = deriveElementEncoder
    implicit val bazEncoder: ElementEncoder[Baz]   = deriveElementEncoderConfigured(config)
    implicit val baz1Decoder: ElementDecoder[Baz1] = deriveElementDecoder
    implicit val baz2Decoder: ElementDecoder[Baz2] = deriveElementDecoder
    implicit val baz3Decoder: ElementDecoder[Baz3] = deriveElementDecoder
    implicit val bazDecoder: ElementDecoder[Baz]   = deriveElementDecoderConfigured(config)
  }

  sealed trait Mammalia {
    def name: String
    def strength: Double
  }

  case class CanisLupus(name: String, strength: Double, age: Int)       extends Mammalia
  case class PantheraLeo(name: String, strength: Double, speed: Double) extends Mammalia

  object Mammalia {
    val config                                                = ElementCodecConfig.default.withConstructorsRenamed(snakeCase)
    implicit val canisEncoder: ElementEncoder[CanisLupus]     = deriveElementEncoder
    implicit val pantheraEncoder: ElementEncoder[PantheraLeo] = deriveElementEncoder
    implicit val mammaliaEncoder: ElementEncoder[Mammalia]    = deriveElementEncoderConfigured(config)
    implicit val canisDecoder: ElementDecoder[CanisLupus]     = deriveElementDecoder
    implicit val pantheraDecoder: ElementDecoder[PantheraLeo] = deriveElementDecoder
    implicit val mammaliaDecoder: ElementDecoder[Mammalia]    = deriveElementDecoderConfigured(config)
  }

  sealed trait Insecta

  @discriminator("hornet")
  case class Vespa(name: String, damage: Double) extends Insecta
  @discriminator("cockroach")
  case class Blattodea(name: String, legsNumber: Int) extends Insecta

  object Insecta {
    implicit val vespaEncoder: ElementEncoder[Vespa]         = deriveElementEncoder
    implicit val blattodeaEncoder: ElementEncoder[Blattodea] = deriveElementEncoder
    implicit val insectaEncoder: ElementEncoder[Insecta]     = deriveElementEncoder
    implicit val vespaDecoder: ElementDecoder[Vespa]         = deriveElementDecoder
    implicit val blattodeaDecoder: ElementDecoder[Blattodea] = deriveElementDecoder
    implicit val insectaDecoder: ElementDecoder[Insecta]     = deriveElementDecoder
  }

  sealed trait Pisces

  @discriminator("ClownFish")
  case class Amphiprion(name: String, finNumber: Int)               extends Pisces
  case class CarcharodonCarcharias(name: String, teethNumber: Long) extends Pisces

  object Pisces {
    val config                                                             = ElementCodecConfig.default.withConstructorsRenamed(snakeCase)
    implicit val amphiprionEncoder: ElementEncoder[Amphiprion]             = deriveElementEncoder
    implicit val carcharodonEncoder: ElementEncoder[CarcharodonCarcharias] = deriveElementEncoder
    implicit val piscesEncoder: ElementEncoder[Pisces]                     = deriveElementEncoderConfigured(config)
    implicit val amphiprionDecoder: ElementDecoder[Amphiprion]             = deriveElementDecoder
    implicit val carcharodonDecoder: ElementDecoder[CarcharodonCarcharias] = deriveElementDecoder
    implicit val piscesDecoder: ElementDecoder[Pisces]                     = deriveElementDecoderConfigured(config)
  }

  sealed trait Animal

  case class Cat(meow: String) extends Animal
  case class Dog(woof: Int)    extends Animal
  case class Cow(moo: Double)  extends Animal

  object Animal {
    val config                                         = ElementCodecConfig.default.usingElementNamesAsDiscriminator.withConstructorsRenamed(snakeCase)
    implicit val catEncoder: ElementEncoder[Cat]       = deriveElementEncoder
    implicit val dogEncoder: ElementEncoder[Dog]       = deriveElementEncoder
    implicit val cowEncoder: ElementEncoder[Cow]       = deriveElementEncoder
    implicit val animalEncoder: ElementEncoder[Animal] = deriveElementEncoderConfigured(config)
    implicit val catDecoder: ElementDecoder[Cat]       = deriveElementDecoder
    implicit val dogDecoder: ElementDecoder[Dog]       = deriveElementDecoder
    implicit val cowDecoder: ElementDecoder[Cow]       = deriveElementDecoder
    implicit val animalDecoder: ElementDecoder[Animal] = deriveElementDecoderConfigured(config)
  }
}
