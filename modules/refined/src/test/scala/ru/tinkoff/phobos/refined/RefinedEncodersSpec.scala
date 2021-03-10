package ru.tinkoff.phobos.refined

import eu.timepit.refined.api.Refined
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import ru.tinkoff.phobos.annotations.{ElementCodec, XmlCodec}
import eu.timepit.refined.refineMV
import eu.timepit.refined.string.MatchesRegex
import eu.timepit.refined.types.numeric.NonNegLong
import ru.tinkoff.phobos.encoding.XmlEncoder
import ru.tinkoff.phobos.syntax.{attr, text}
import shapeless.{Witness => W}
import ru.tinkoff.phobos.testString._

class RefinedEncodersSpec extends AnyWordSpec with Matchers {
  type NumericAtLeastTo = MatchesRegex[W.`"[0-9]{2,}"`.T]

  @XmlCodec("test")
  case class Test(x: Int, y: Refined[String, NumericAtLeastTo])

  @ElementCodec
  case class Foo(@attr bar: Int, @text baz: NonNegLong)
  @XmlCodec("qux")
  case class Qux(str: String, foo: Foo)

  "refined encoder" should {
    "encode element correctly" in {
      val value = Test(2, refineMV[NumericAtLeastTo]("123"))

      val expectedResult = """
         | <?xml version='1.0' encoding='UTF-8'?>
         | <test>
         |   <x>2</x>
         |   <y>123</y>
         | </test>
          """.stripMargin.minimized

      XmlEncoder[Test].encode(value) shouldEqual expectedResult
    }

    "encode text correctly" in {
      val qux = Qux("42", Foo(42, NonNegLong(1000L)))
      val xml = XmlEncoder[Qux].encode(qux)
      assert(
        xml ==
          """
            | <?xml version='1.0' encoding='UTF-8'?>
            | <qux>
            |   <str>42</str>
            |   <foo bar="42">1000</foo>
            | </qux>
          """.stripMargin.minimized,
      )
    }
  }
}
