package ru.tinkoff.phobos.ast

import org.scalacheck.Prop.forAll
import org.scalacheck.Properties
import ru.tinkoff.phobos.decoding.XmlDecoder
import ru.tinkoff.phobos.encoding.XmlEncoder
import org.scalacheck.{Arbitrary, Gen}

class CodecProperties extends Properties("Ast codecs") {
  import CodecProperties._

  private val encoder = XmlEncoder.fromElementEncoder[XmlEntry]("test")
  private val decoder = XmlDecoder.fromElementDecoder[XmlEntry]("test")

  property("decode(encode(ast)) === ast") = forAll { entry: XmlEntry =>
    decoder.decode(
      encoder.encode(entry),
    ) == Right(entry)
  }

  property("encode(decode(xmlAst)) === xmlAst") = forAll { entry: XmlEntry =>
    val encoded = encoder.encode(entry)

    decoder.decode(encoded).map(encoder.encode(_)) == Right(encoded)
  }
}

object CodecProperties {
  implicit val arbitraryXmlLong: Arbitrary[XmlNumber.Aux[Long]] = Arbitrary(
    Arbitrary.arbitrary[Long].map(XmlNumber.integral),
  )

  implicit val arbitraryXmlDouble: Arbitrary[XmlNumber.Aux[Double]] = Arbitrary(
    Arbitrary.arbitrary[Double].map(XmlNumber.double),
  )

  implicit val arbitraryXmlNumber: Arbitrary[XmlNumber] =
    Arbitrary(
      Gen.oneOf(arbitraryXmlLong.arbitrary, arbitraryXmlDouble.arbitrary),
    )

  implicit val arbitraryXmlBoolean: Arbitrary[XmlBoolean] = Arbitrary(
    Arbitrary.arbitrary[Boolean].map(XmlBoolean.fromBoolean),
  )

  implicit val arbNonEmptyString: Arbitrary[String] = Arbitrary {
    Gen.choose(1, 10).flatMap { n =>
      Gen
        .containerOfN[List, Char](n, Gen.oneOf("abcdefghijklmnopqrstuvwxyz".toList))
        .map(_.mkString)
    }
  }

  implicit val arbitraryXmlText: Arbitrary[XmlText] = Arbitrary(
    arbNonEmptyString.arbitrary.map(XmlText(_)),
  )

  implicit val arbitraryXmlLeaf: Arbitrary[XmlLeaf] = Arbitrary(
    Gen.oneOf(arbitraryXmlNumber.arbitrary, arbitraryXmlBoolean.arbitrary, arbitraryXmlText.arbitrary),
  )

  class Depth(val value: Int) extends AnyVal

  def arbitraryXmlNode(depth: Depth): Arbitrary[XmlNode] = Arbitrary {
    val arbInt           = Gen.choose(0, 5)
    def arbNames(n: Int) = Gen.containerOfN[Set, String](n, arbNonEmptyString.arbitrary)

    def arbLeafs(n: Int) =
      for {
        names <- arbNames(n)
        leafs <- Gen.containerOfN[List, XmlLeaf](n, arbitraryXmlLeaf.arbitrary)
      } yield names.toList zip leafs

    val arbNodes: Gen[List[(String, XmlEntry)]] = arbInt.flatMap { n =>
      if (depth.value > 3) arbLeafs(n)
      else {
        val depth2: Depth = new Depth(depth.value + 1)
        val arbEntries = Gen
          .containerOfN[List, XmlEntry](n, arbitraryXmlNode(depth2).arbitrary)

        for {
          names   <- arbNames(n)
          entries <- arbEntries
        } yield names.toList zip entries
      }
    }
    for {
      nAttrs <- arbInt
      attrs  <- arbLeafs(nAttrs)
      nodes  <- arbNodes
    } yield XmlNode(attrs, nodes)
  }

  implicit val arbitraryXmlEntry: Arbitrary[XmlEntry] =
    Arbitrary(arbitraryXmlNode(new Depth(0)).arbitrary)
}
