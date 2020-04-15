package ru.tinkoff.phobos.ast

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import ru.tinkoff.phobos.Namespace
import ru.tinkoff.phobos.decoding.XmlDecoder

class XmlEntryElementDecoderSpec extends AnyWordSpec with Matchers {
  "XmlEntry decoder" should {
    "decodes simple Xml into ast correctly" in {
      val xml        = """<?xml version='1.0' encoding='UTF-8'?><ast foo="5"><bar>bazz</bar></ast>"""
      val decodedAst = XmlDecoder.fromElementDecoder[XmlEntry]("ast").decode(xml)
      assert(
        decodedAst.contains(
          XmlNode
            .withAttributes("foo" -> 5)
            .withChildren(
              "bar" -> "bazz"
            )
        )
      )
    }

    "decodes complicated Xml into ast correctly" in {
      case object tinkoff {
        type ns = tinkoff.type
        implicit val ns: Namespace[tinkoff.type] = Namespace.mkInstance("https://tinkoff.ru")
      }

      val xml =
        """<?xml version='1.0' encoding='UTF-8'?><ans1:ast xmlns:ans1="https://tinkoff.ru" foo="5"><bar>bazz</bar><array foo2="true" foo3="false"><elem>11111111111111</elem><elem>11111111111112</elem></array><nested><scala>2.13</scala><dotty>0.13</dotty><scala-4/></nested></ans1:ast>"""

      val decodedAst = XmlDecoder.fromElementDecoderNs[XmlEntry, tinkoff.ns]("ast").decode(xml)
      assert(
        decodedAst.contains(
          XmlNode
            .withAttributes("foo" -> 5)
            .withChildren(
              "bar" -> "bazz",
              "array" -> XmlNode
                .withAttributes("foo2" -> true, "foo3" -> false)
                .withChildren(
                  "elem" -> 11111111111111L,
                  "elem" -> 11111111111112L
                ),
              "nested" -> XmlNode
                .withChildren(
                  "scala"   -> 2.13,
                  "dotty"   -> 0.13,
                  "scala-4" -> XmlNode.empty
                )
            )
        )
      )
    }
  }
}
