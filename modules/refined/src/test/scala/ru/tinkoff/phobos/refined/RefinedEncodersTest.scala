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

class RefinedEncodersTest extends AnyWordSpec with Matchers {
  type NumericAtLeastTwo = MatchesRegex[W.`"[0-9]{2,}"`.T]

  "refined encoder" should {

    "encode attributes correctly" in {
      @XmlCodec("test")
      case class Test(x: Int, @attr y: Refined[String, NumericAtLeastTwo])

      val value = Test(2, refineMV[NumericAtLeastTwo]("123"))

      val expectedResult = """
         | <?xml version='1.0' encoding='UTF-8'?>
         | <test y="123">
         |   <x>2</x>
         | </test>
       """.stripMargin.minimized

      XmlEncoder[Test].encode(value) shouldEqual Right(expectedResult)
    }

    "encode elements correctly" in {
      @XmlCodec("test")
      case class Test(x: Int, y: Refined[String, NumericAtLeastTwo])

      val value = Test(2, refineMV[NumericAtLeastTwo]("123"))

      val expectedResult = """
         | <?xml version='1.0' encoding='UTF-8'?>
         | <test>
         |   <x>2</x>
         |   <y>123</y>
         | </test>
        """.stripMargin.minimized

      XmlEncoder[Test].encode(value) shouldEqual Right(expectedResult)
    }

    "encode text correctly" in {
      @ElementCodec
      case class Foo(@attr bar: Int, @text baz: NonNegLong)
      @XmlCodec("qux")
      case class Qux(str: String, foo: Foo)

      val qux = Qux("42", Foo(42, NonNegLong(1000L)))
      val xml = XmlEncoder[Qux].encode(qux)
      val string =
        """
            | <?xml version='1.0' encoding='UTF-8'?>
            | <qux>
            |   <str>42</str>
            |   <foo bar="42">1000</foo>
            | </qux>
          """.stripMargin.minimized
      assert(xml == Right(string))
    }
  }
}
