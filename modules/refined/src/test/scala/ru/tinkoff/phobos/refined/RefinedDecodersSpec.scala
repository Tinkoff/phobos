package ru.tinkoff.phobos.refined

import eu.timepit.refined.api.Refined
import eu.timepit.refined.refineMV
import eu.timepit.refined.string.MatchesRegex
import eu.timepit.refined.types.numeric.NonNegLong
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import ru.tinkoff.phobos.annotations.{ElementCodec, XmlCodec}
import ru.tinkoff.phobos.decoding.XmlDecoder
import ru.tinkoff.phobos.syntax.{attr, text}
import ru.tinkoff.phobos.testString._
import shapeless.{Witness => W}

class RefinedDecodersSpec extends AnyWordSpec with Matchers {
  type NumericAtLeastTo = MatchesRegex[W.`"[0-9]{2,}"`.T]

  @XmlCodec("test")
  case class Test(x: Int, y: Refined[String, NumericAtLeastTo])

  @ElementCodec
  case class Foo(@attr bar: Int, @text baz: NonNegLong)
  @XmlCodec("qux")
  case class Qux(str: String, foo: Foo)

  "refined decoder" should {
    "decode element correctly" in {

      val sampleXml = """
         | <?xml version='1.0' encoding='UTF-8'?>
         | <test>
         |   <x>2</x>
         |   <y>123</y>
         | </test>
          """.stripMargin.minimized

      val expectedResult = Test(2, refineMV[NumericAtLeastTo]("123"))

      XmlDecoder[Test].decode(sampleXml) shouldEqual (Right(expectedResult))

    }

    "decode text correctly" in {
      val sampleXml =
        """
          | <?xml version='1.0' encoding='UTF-8'?>
          | <qux>
          |   <str>42</str>
          |   <foo bar="42">1000</foo>
          | </qux>
          """.stripMargin.minimized

      val expectedResult = Qux("42", Foo(42, NonNegLong(1000L)))
      XmlDecoder[Qux].decode(sampleXml) shouldEqual Right(expectedResult)
    }

    "provide verbose errorst" in {

      @XmlCodec("test")
      case class Test2(x: Int, y: Refined[String, NumericAtLeastTo])
      @ElementCodec
      case class Foo2(@attr bar: Int, @text baz: NonNegLong)
      @XmlCodec("qux")
      case class Qux2(str: String, foo: Foo2)

      val sampleXml0 = """
           | <?xml version='1.0' encoding='UTF-8'?>
           | <test>
           |   <x>2</x>
           |   <y>1</y>
           | </test>
          """.stripMargin.minimized

      XmlDecoder[Test2]
        .decode(sampleXml0)
        .left
        .map(_.text) shouldEqual Left(
        """Failed to verify RefinedDecodersSpec.this.NumericAtLeastTo refinement for value=1 of raw type String: Predicate failed: "1".matches("[0-9]{2,}").""",
      )

      val sampleXml1 =
        """
          | <?xml version='1.0' encoding='UTF-8'?>
          | <qux>
          |   <str>42</str>
          |   <foo bar="42">-1000</foo>
          | </qux>
          """.stripMargin.minimized

      XmlDecoder[Qux2]
        .decode(sampleXml1)
        .left
        .map(_.text) shouldEqual Left(
        """Failed to verify eu.timepit.refined.numeric.NonNegative refinement for value=-1000 of raw type Long: Predicate (-1000 < 0) did not fail.""",
      )

    }
  }
}
