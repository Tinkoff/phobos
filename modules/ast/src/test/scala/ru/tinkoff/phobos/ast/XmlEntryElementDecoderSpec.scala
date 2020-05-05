package ru.tinkoff.phobos.ast

import com.softwaremill.diffx.scalatest.DiffMatcher
import org.scalatest.EitherValues
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import ru.tinkoff.phobos.Namespace
import ru.tinkoff.phobos.decoding.XmlDecoder

class XmlEntryElementDecoderSpec extends AnyWordSpec with Matchers with DiffMatcher with EitherValues {

  "XmlEntry decoder" should {
    "decodes simple Xml into ast correctly" in {
      val sampleXml                = """<?xml version='1.0' encoding='UTF-8'?><ast foo="5"><bar>bazz</bar></ast>"""
      val decodedAst               = XmlDecoder.fromElementDecoder[XmlEntry]("ast").decode(sampleXml).right.value
      val expectedResult: XmlEntry = xml(attr("foo") := 5, node("bar") := "bazz")

      decodedAst should matchTo(expectedResult)
    }

    "decodes complicated Xml into ast correctly" in {
      case object tinkoff {
        type ns = tinkoff.type
        implicit val ns: Namespace[tinkoff.type] = Namespace.mkInstance("https://tinkoff.ru")
      }

      val sampleXml =
        """<?xml version='1.0' encoding='UTF-8'?><ans1:ast xmlns:ans1="https://tinkoff.ru" foo="5"><bar>bazz</bar><array foo2="true" foo3="false"><elem>11111111111111</elem><elem>11111111111112</elem></array><nested><scala>2.13</scala><dotty>0.13</dotty><scala-4/></nested></ans1:ast>"""

      val decodedAst = XmlDecoder.fromElementDecoderNs[XmlEntry, tinkoff.ns]("ast").decode(sampleXml).right.value
      val expectedResult: XmlEntry = xml(attr("foo") := 5)(
        node("bar") := "bazz",
        node("array") := xml(
          attr("foo2") := true,
          attr("foo3") := false
        )(
          node("elem") := 11111111111111L,
          node("elem") := 11111111111112L
        ),
        node("nested") := xml(
          node("scala") := 2.13,
          node("dotty") := 0.13,
          node("scala-4") := xml.empty
        )
      )

      decodedAst should matchTo(expectedResult)
    }

    "works fine when for elements with same name" in {

      val n: XmlEntry = xml(
        node("k") :=
          xml(
            node("k") := "gbq" // when node name != parent node name - everything works fine
          )
      )

      val encoded = ru.tinkoff.phobos.encoding.XmlEncoder.fromElementEncoder[XmlEntry]("ast").encode(n)

      val result = XmlDecoder
        .fromElementDecoder[XmlEntry]("ast")
        .decode(
          encoded
        )
        .right
        .get

      util.AstTransformer.sortNodeValues(result) should matchTo(
        util.AstTransformer.sortNodeValues(n)
      )
    }
  }
}
