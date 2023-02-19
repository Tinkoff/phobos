package ru.tinkoff.phobos

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import ru.tinkoff.phobos.decoding._
import ru.tinkoff.phobos.encoding._
import ru.tinkoff.phobos.testString._

class DerivationTest extends AnyWordSpec with Matchers {
  "ElementEncoder.derived" should {
    "derive simple encoders" in {
      case class Foo(a: Int, b: String, c: Double) derives ElementEncoder
      case class Bar(d: String, foo: Foo, e: Char) derives ElementEncoder
      given XmlEncoder[Bar] = XmlEncoder.fromElementEncoder("bar")

      val bar    = Bar("d value", Foo(1, "b value", 3.0), 'e')
      val string = """<?xml version='1.0' encoding='UTF-8'?>
                     | <bar>
                     |   <d>d value</d>
                     |   <foo>
                     |     <a>1</a>
                     |     <b>b value</b>
                     |     <c>3.0</c>
                     |   </foo>
                     |   <e>e</e>
                     | </bar>
                   """.stripMargin.minimized

      val encoded = XmlEncoder[Bar].encode(bar)
      assert(encoded == Right(string))
    }
  }

  "ElementDecoder.derived" should {
    "derive simple decoders" in {
      case class Foo(a: Int, b: String, c: Double) derives ElementDecoder
      case class Bar(d: String, foo: Foo, e: Char) derives ElementDecoder
      given XmlDecoder[Bar] = XmlDecoder.fromElementDecoder("bar")

      val bar    = Bar("d value", Foo(1, "b value", 3.0), 'e')
      val string = """<?xml version='1.0' encoding='UTF-8'?>
                     | <bar>
                     |   <d>d value</d>
                     |   <foo>
                     |     <a>1</a>
                     |     <b>b v<![CDATA[al]]>ue</b>
                     |     <c>3.0</c>
                     |   </foo>
                     |   <e>e</e>
                     | </bar>
                   """.stripMargin

      val decoded = XmlDecoder[Bar].decode(string)
      assert(decoded == Right(bar))
    }
  }

}