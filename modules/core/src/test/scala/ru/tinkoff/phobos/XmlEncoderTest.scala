package ru.tinkoff.phobos

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import ru.tinkoff.phobos.derivation.semiauto.deriveXmlEncoder
import ru.tinkoff.phobos.encoding.XmlEncoder

class XmlEncoderTest extends AnyWordSpec with Matchers {
  "XmlEncoder with config" should {
    "ignore prolog if configured" in {
      final case class Foo(a: Int, b: String, c: Double)
      implicit val fooEncoder: XmlEncoder[Foo] = deriveXmlEncoder("Foo")

      XmlEncoder[Foo].encodeWithConfig(Foo(1, "abc", 1.0), XmlEncoder.defaultConfig.withoutProlog) shouldBe
        Right("<Foo><a>1</a><b>abc</b><c>1.0</c></Foo>")
    }

    "not ignore prolog by default" in {
      final case class Foo(a: Int, b: String, c: Double)
      implicit val fooEncoder: XmlEncoder[Foo] = deriveXmlEncoder("Foo")

      XmlEncoder[Foo].encodeWithConfig(Foo(1, "abc", 1.0), XmlEncoder.defaultConfig) shouldBe
        Right("<?xml version='1.0' encoding='UTF-8'?><Foo><a>1</a><b>abc</b><c>1.0</c></Foo>")
    }

    "overwrite prolog information if configured" in {
      final case class Foo(a: Int, b: String, c: Double)
      implicit val fooEncoder: XmlEncoder[Foo] = deriveXmlEncoder("Foo")

      XmlEncoder[Foo].encodeWithConfig(Foo(1, "abc", 1.0), XmlEncoder.XmlEncoderConfig("UTF-16", "1.1", true)) shouldBe
        Right("<?xml version='1.1' encoding='UTF-16'?><Foo><a>1</a><b>abc</b><c>1.0</c></Foo>")
    }
  }
}
