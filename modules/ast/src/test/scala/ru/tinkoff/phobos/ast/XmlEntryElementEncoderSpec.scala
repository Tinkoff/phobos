package ru.tinkoff.phobos.ast

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import ru.tinkoff.phobos.Namespace
import ru.tinkoff.phobos.encoding.XmlEncoder

class XmlEntryElementEncoderSpec extends AnyWordSpec with Matchers {
  "XmlEntry encoder" should {
    "encodes simple Xml ast correctly" in {
      val ast = XmlNode
        .withAttributes("foo" -> 5)
        .withChildren(
          "bar" -> "bazz"
        )

      val result =
        XmlEncoder
          .fromElementEncoder[XmlEntry]("ast")
          .encode(ast)

      assert(result == """<?xml version='1.0' encoding='UTF-8'?><ast foo="5"><bar>bazz</bar></ast>""")
    }
    "encodes nested Xml ast correctly" in {
      case object tinkoff {
        type ns = tinkoff.type
        implicit val ns: Namespace[tinkoff.type] = Namespace.mkInstance("https://tinkoff.ru")
      }

      val ast = XmlNode
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

      val result =
        XmlEncoder
          .fromElementEncoderNs[XmlEntry, tinkoff.ns]("ast")
          .encode(ast)

      assert(
        result == """<?xml version='1.0' encoding='UTF-8'?><ans1:ast xmlns:ans1="https://tinkoff.ru" foo="5"><bar>bazz</bar><array foo2="true" foo3="false"><elem>11111111111111</elem><elem>11111111111112</elem></array><nested><scala>2.13</scala><dotty>0.13</dotty><scala-4/></nested></ans1:ast>"""
      )
    }
  }
}
