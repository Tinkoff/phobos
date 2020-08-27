package ru.tinkoff.phobos.akka_http

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

class SoapSuit extends AnyWordSpec with Matchers {
  "Soap codecs" should {
    "be found for envelope" in {
      """
       | import ru.tinkoff.phobos.encoding.XmlEncoder
       | import ru.tinkoff.phobos.decoding.XmlDecoder
       | import ru.tinkoff.phobos.annotations.ElementCodec
       | @ElementCodec
       | case class Header(foo: Int)
       | @ElementCodec
       | case class Body(bar: String)
       | implicitly[XmlEncoder[Envelope[Header, Body]]]
       | implicitly[XmlDecoder[Envelope[Header, Body]]]
      """.stripMargin should compile
    }

    "be found for headless envelope" in {
      """
       | import ru.tinkoff.phobos.encoding.XmlEncoder
       | import ru.tinkoff.phobos.decoding.XmlDecoder
       | import ru.tinkoff.phobos.annotations.ElementCodec
       | @ElementCodec
       | case class Body(bar: String)
       | implicitly[XmlEncoder[HeadlessEnvelope[Body]]]
       | implicitly[XmlDecoder[HeadlessEnvelope[Body]]]
      """.stripMargin should compile
    }
  }
}
