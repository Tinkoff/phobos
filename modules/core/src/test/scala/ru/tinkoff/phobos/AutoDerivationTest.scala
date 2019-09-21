package ru.tinkoff.phobos

import ru.tinkoff.phobos.testString._
import org.scalatest.{Matchers, WordSpec}

class AutoDerivationTest extends WordSpec with Matchers {
  "Automatic derivation" should {
    "derive codecs" in {
      """
        | import ru.tinkoff.phobos.encoding._
        | import ru.tinkoff.phobos.decoding._
        | import ru.tinkoff.phobos.syntax._
        | import ru.tinkoff.phobos.derivation.auto._
        |
        | case class Foo(@attr bar: Int, @attr baz: Double, @text txt: String)
        | case class Bar(@attr quxx: Float, foo: Foo, qux: Byte)
        |
        | implicitly[ElementEncoder[Bar]]
        | implicitly[ElementDecoder[Bar]]
        |""".stripMargin should compile
    }

    "not derive encoder if not imported" in {
      """
        | import ru.tinkoff.phobos.encoding._
        | import ru.tinkoff.phobos.syntax._
        | case class Foo(@attr bar: Int, @attr baz: Double, @text txt: String)
        | case class Bar(@attr quxx: Float, foo: Foo, qux: Byte)
        |
        | implicitly[ElementEncoder[Bar]]
        |""".stripMargin shouldNot compile
    }


    "not derive decoder if not imported" in {
      """
        | import ru.tinkoff.phobos.decoding._
        | import ru.tinkoff.phobos.syntax._
        |
        | case class Foo(@attr bar: Int, @attr baz: Double, @text txt: String)
        | case class Bar(@attr quxx: Float, foo: Foo, qux: Byte)
        |
        | implicitly[ElementDecoder[Bar]]
        |""".stripMargin shouldNot compile
    }

    "derive codecs correctly" in {
      import ru.tinkoff.phobos.syntax._
      import ru.tinkoff.phobos.decoding._
      import ru.tinkoff.phobos.encoding._
      import ru.tinkoff.phobos.derivation.auto._

      case class Foo(@attr bar: Int, @attr baz: Double, @text txt: String)
      case class Bar(foo: Float)
      case class Baz(foo: Foo, bars: List[Bar], maybebar: Option[Bar])

      val baz = Baz(
        foo = Foo(42, 144.12, "keke"),
        bars = List(Bar(1), Bar(2), Bar(3)),
        maybebar = Some(Bar(4))
      )
      val bazXml =
      """<?xml version='1.0' encoding='UTF-8'?>
       | <baz>
       |     <foo bar="42" baz="144.12">keke</foo>
       |     <bars><foo>1.0</foo></bars>
       |     <bars><foo>2.0</foo></bars>
       |     <bars><foo>3.0</foo></bars>
       |     <maybebar><foo>4.0</foo></maybebar>
       | </baz>
       |""".stripMargin

      val encoder = XmlEncoder.fromElementEncoder[Baz]("baz")
      assert(encoder.encode(baz) == bazXml.minimized)

      val decoder = derivation.semiauto.deriveElementDecoder[Baz]
//      implicitly[Exported[ElementDecoder[Baz]]]
//      val decoder = XmlDecoder.fromElementDecoder[Baz]("baz")
//      assert(decoder.decode(bazXml) == baz)
    }
  }
}