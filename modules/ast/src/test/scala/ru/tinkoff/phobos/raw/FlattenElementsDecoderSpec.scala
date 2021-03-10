package ru.tinkoff.phobos.raw

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import ru.tinkoff.phobos.Namespace
import ru.tinkoff.phobos.decoding.XmlDecoder
import ru.tinkoff.phobos.ast._

class FlattenElementsDecoderSpec extends AnyWordSpec with Matchers {

  "XmlEntry decoder" should {
    "decodes simple Xml into ast correctly" in {
      val xml        = """<?xml version='1.0' encoding='UTF-8'?><ast foo="5"><bar>bazz</bar></ast>"""
      val decodedRaw = XmlDecoder.fromElementDecoder[ElementsFlatten]("ast").decode(xml)
      assert(
        decodedRaw.contains(
          ElementsFlatten(
            "bar" -> "bazz",
          ),
        ),
      )
    }

    "decodes complicated Xml into ast correctly" in {
      case object tinkoff {
        type ns = tinkoff.type
        implicit val ns: Namespace[tinkoff.type] = Namespace.mkInstance("https://tinkoff.ru")
      }

      val xml =
        """<?xml version='1.0' encoding='UTF-8'?><ans1:ast xmlns:ans1="https://tinkoff.ru" foo="5"><bar>bazz</bar><array foo2="true" foo3="false"><elem>11111111111111</elem><elem>11111111111112</elem></array><nested><scala>2.13</scala><dotty>0.13</dotty><scala-4/></nested></ans1:ast>"""

      val decodedRaw = XmlDecoder.fromElementDecoderNs[ElementsFlatten, tinkoff.ns]("ast").decode(xml)
      assert(
        decodedRaw.contains(
          ElementsFlatten(
            "bar"   -> "bazz",
            "elem"  -> 11111111111111L,
            "elem"  -> 11111111111112L,
            "scala" -> 2.13,
            "dotty" -> 0.13,
          ),
        ),
      )
    }
  }
}
