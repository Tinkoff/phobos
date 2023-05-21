package ru.tinkoff.phobos

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import ru.tinkoff.phobos.decoding._
import ru.tinkoff.phobos.encoding._
import ru.tinkoff.phobos.testString._
import ru.tinkoff.phobos.syntax.discriminator
import ru.tinkoff.phobos.syntax.text
import ru.tinkoff.phobos.syntax.attr
import ru.tinkoff.phobos.SealedClasses.Animal.animalDecoder
import ru.tinkoff.phobos.derivation.LazySummon
import scala.reflect.TypeTest
import ru.tinkoff.phobos.configured.ElementCodecConfig
import ru.tinkoff.phobos.derivation.common.extractSumTypeChild
import ru.tinkoff.phobos.derivation.semiauto.deriveXmlEncoder
import ru.tinkoff.phobos.derivation.encoder.deriveElementEncoder
import scala.deriving.Mirror
import scala.annotation.nowarn

class DerivationTest extends AnyWordSpec with Matchers {
  import DerivationTest.*

  "ElementEncoder.derived" should {
    "derive for products" in {
      given XmlEncoder[Bar] = XmlEncoder.fromElementEncoder("bar")

      val bar = Bar("d value", Foo(1, "b value", 3.0), 'e')
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

    "derive for sealed traits" in {
      given XmlEncoder[Wild] = XmlEncoder.fromElementEncoder("Wild")
      assert(XmlEncoder[Wild].encode(Wild.Tiger) == Right("""
        | <?xml version='1.0' encoding='UTF-8'?>
        | <Wild xmlns:ans1="http://www.w3.org/2001/XMLSchema-instance" ans1:type="cat"/>
        """.stripMargin.minimized))
      assert(XmlEncoder[Wild].encode(Wild.Wolf("Coyote")) == Right("""
        | <?xml version='1.0' encoding='UTF-8'?>
        | <Wild xmlns:ans1="http://www.w3.org/2001/XMLSchema-instance" ans1:type="dog">Coyote</Wild>
        """.stripMargin.minimized))
    }

    "derive for enums" in {
      given XmlEncoder[Domestic] = XmlEncoder.fromElementEncoder("Domestic")
      assert(XmlEncoder[Domestic].encode(Domestic.Cat) == Right("""
        | <?xml version='1.0' encoding='UTF-8'?>
        | <Domestic xmlns:ans1="http://www.w3.org/2001/XMLSchema-instance" ans1:type="tiger"/>
        """.stripMargin.minimized))
      assert(XmlEncoder[Domestic].encode(Domestic.Dog("Pug")) == Right("""
        | <?xml version='1.0' encoding='UTF-8'?>
        | <Domestic xmlns:ans1="http://www.w3.org/2001/XMLSchema-instance" ans1:type="wolf">Pug</Domestic>
        """.stripMargin.minimized))
    }

    "derive for products with sealed traits and enums" in {
      given XmlEncoder[Nature] = XmlEncoder.fromElementEncoder("Nature")

      val res = XmlEncoder[Nature].encode(Nature(Wild.Tiger, Domestic.Dog("Pug")))
      assert(
        res == Right(
          """
          | <?xml version='1.0' encoding='UTF-8'?>
          | <Nature>
          |   <wild xmlns:ans1="http://www.w3.org/2001/XMLSchema-instance" ans1:type="cat"/>
          |   <domestic xmlns:ans2="http://www.w3.org/2001/XMLSchema-instance" ans2:type="wolf">Pug</domestic>
          | </Nature>
          """.stripMargin.minimized,
        ),
      )
    }
  }

  import scala.compiletime.*
  import scala.deriving.*

  "ElementDecoder.derived" should {
    "derive for products" in {
      given XmlDecoder[Bar] = XmlDecoder.fromElementDecoder("bar")

      val bar = Bar("d value", Foo(1, "b value", 3.0), 'e')
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

    "derive for sealed traits" in {
      given XmlDecoder[Wild] = XmlDecoder.fromElementDecoder("Wild")

      val tigerString = """<?xml version='1.0' encoding='UTF-8'?>
        | <Wild xmlns:ans1="http://www.w3.org/2001/XMLSchema-instance" ans1:type="cat"/>
        """.stripMargin

      val wolfString = """<?xml version='1.0' encoding='UTF-8'?>
          | <Wild xmlns:ans1="http://www.w3.org/2001/XMLSchema-instance" ans1:type="dog">Coyote</Wild>
          """.stripMargin

      assert(XmlDecoder[Wild].decode(tigerString) == Right(Wild.Tiger))
      assert(XmlDecoder[Wild].decode(wolfString) == Right(Wild.Wolf("Coyote")))
    }

    "derive for enums" in {
      given XmlDecoder[Domestic] = XmlDecoder.fromElementDecoder("Domestic")

      val catString = """<?xml version='1.0' encoding='UTF-8'?>
      | <Domestic xmlns:ans1="http://www.w3.org/2001/XMLSchema-instance" ans1:type="tiger"/>
      """.stripMargin

      val dogString = """<?xml version='1.0' encoding='UTF-8'?>
      | <Domestic xmlns:ans1="http://www.w3.org/2001/XMLSchema-instance" ans1:type="wolf">Pug</Domestic>
      """.stripMargin

      assert(XmlDecoder[Domestic].decode(catString) == Right(Domestic.Cat))
      assert(XmlDecoder[Domestic].decode(dogString) == Right(Domestic.Dog("Pug")))
    }

    "derive for products with sealed traits and enums" in {
      given XmlDecoder[Nature] = XmlDecoder.fromElementDecoder("Nature")

      val natureString = """
          | <?xml version='1.0' encoding='UTF-8'?>
          | <Nature>
          |   <wild xmlns:ans1="http://www.w3.org/2001/XMLSchema-instance" ans1:type="cat"/>
          |   <domestic xmlns:ans2="http://www.w3.org/2001/XMLSchema-instance" ans2:type="wolf">Pug</domestic>
          | </Nature>
          """.stripMargin.minimized

      val nature = Nature(Wild.Tiger, Domestic.Dog("Pug"))
      assert(XmlDecoder[Nature].decode(natureString) == Right(nature))
    }
  }
}

object DerivationTest {
  case class Foo(a: Int, b: String, c: Double) derives ElementEncoder, ElementDecoder
  case class Bar(d: String, foo: Foo, e: Char) derives ElementEncoder, ElementDecoder

  sealed trait Wild derives ElementEncoder, ElementDecoder
  object Wild {
    @discriminator("cat") case object Tiger                    extends Wild
    @discriminator("dog") case class Wolf(@text breed: String) extends Wild
  }

  enum Domestic derives ElementEncoder, ElementDecoder {
    @discriminator("tiger") case Cat
    @discriminator("wolf") case Dog(@text breed: String)
  }

  case class Nature(wild: Wild, domestic: Domestic) derives ElementEncoder, ElementDecoder
}
