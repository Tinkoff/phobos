package ru.tinkoff.phobos

import org.scalatest.{Matchers, WordSpec}
import ru.tinkoff.phobos.annotations.XmlCodec
import ru.tinkoff.phobos.encoding.XmlEncoder

class XmlEncoderTest extends WordSpec with Matchers {
  "XmlEncoder with config" should {
    "ignore prolog if configured" in {
      @XmlCodec("Foo")
      final case class Foo(a: Int, b: String, c: Double)

      XmlEncoder[Foo].encodeWithConfig(Foo(1, "abc", 1.0), XmlEncoder.defaultConfig.withoutProlog) shouldBe
        "<Foo><a>1</a><b>abc</b><c>1.0</c></Foo>"
    }
  }
}
