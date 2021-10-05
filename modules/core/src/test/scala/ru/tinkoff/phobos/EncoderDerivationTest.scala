package ru.tinkoff.phobos

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import ru.tinkoff.phobos.SealedClasses.{Animal, Cat, Cow, Dog}
import ru.tinkoff.phobos.annotations.{ElementCodec, XmlCodec, XmlCodecNs, XmlnsDef}
import ru.tinkoff.phobos.encoding.{AttributeEncoder, ElementEncoder, TextEncoder, XmlEncoder}
import ru.tinkoff.phobos.testString._
import ru.tinkoff.phobos.syntax._
import ru.tinkoff.phobos.configured.naming._
import ru.tinkoff.phobos.configured.ElementCodecConfig

class EncoderDerivationTest extends AnyWordSpec with Matchers {

  "Encoder derivation without namespaces" should {
    "encode simple case classes" in {
      @ElementCodec
      case class Foo(a: Int, b: String, c: Double)
      @XmlCodec("bar")
      case class Bar(d: String, foo: Foo, e: Char)

      val bar    = Bar("d value", Foo(1, "b value", 3.0), 'e')
      val string = XmlEncoder[Bar].encode(bar)
      assert(
        string ==
          """
            | <?xml version='1.0' encoding='UTF-8'?>
            | <bar>
            |   <d>d value</d>
            |   <foo>
            |     <a>1</a>
            |     <b>b value</b>
            |     <c>3.0</c>
            |   </foo>
            |   <e>e</e>
            | </bar>
          """.stripMargin.minimized,
      )
    }

    "encode attributes" in {
      @ElementCodec
      case class Foo(a: Int, @attr b: String, c: Double)
      @XmlCodec("bar")
      case class Bar(d: String, foo: Foo, @attr e: Char)

      val bar    = Bar("d value", Foo(1, "b value", 3.0), 'e')
      val string = XmlEncoder[Bar].encode(bar)
      assert(
        string ==
          """
            | <?xml version='1.0' encoding='UTF-8'?>
            | <bar e="e">
            |   <d>d value</d>
            |   <foo b="b value">
            |     <a>1</a>
            |     <c>3.0</c>
            |   </foo>
            | </bar>
          """.stripMargin.minimized,
      )
    }

    "allow to override codecs" in {
      implicit val alternativeElementEncoder: ElementEncoder[String] =
        ElementEncoder.stringEncoder.contramap(_ => "constant")
      implicit val alternativeAttributeEncoder: AttributeEncoder[Int] =
        AttributeEncoder.stringEncoder.contramap(_ => "a74153b")
      implicit val alternativeTextEncoder: TextEncoder[Double] =
        TextEncoder.stringEncoder.contramap(_ => "text")

      @ElementCodec
      case class Foo(@attr bar: Int, @text baz: Double)
      @XmlCodec("qux")
      case class Qux(str: String, foo: Foo)

      val qux = Qux("42", Foo(42, 12.2))
      val xml = XmlEncoder[Qux].encode(qux)
      assert(
        xml ==
          """
          | <?xml version='1.0' encoding='UTF-8'?>
          | <qux>
          |   <str>constant</str>
          |   <foo bar="a74153b">text</foo>
          | </qux>
          """.stripMargin.minimized,
      )
    }

    "encode options" in {
      @ElementCodec
      case class Foo(a: Int, @attr b: String, c: Double)
      @XmlCodec("Wrapper")
      case class Wrapper(foo: Option[Foo])

      val xml1 = XmlEncoder[Wrapper].encode(Wrapper(Some(Foo(1, "b", 2.0))))
      val xml2 = XmlEncoder[Wrapper].encode(Wrapper(None))
      assert(
        xml1 ==
          """
            | <?xml version='1.0' encoding='UTF-8'?>
            | <Wrapper>
            |   <foo b="b">
            |     <a>1</a>
            |     <c>2.0</c>
            |   </foo>
            | </Wrapper>
          """.stripMargin.minimized &&
          xml2 ==
          """
              | <?xml version='1.0' encoding='UTF-8'?>
              | <Wrapper/>
            """.stripMargin.minimized,
      )
    }

    "encode lists" in {
      @ElementCodec
      case class Foo(a: Int, @attr b: Option[String], c: Option[Double])
      @XmlCodec("foos")
      case class Foos(foo: List[Foo])
      val bar1 = Foos(
        List(
          Foo(1, Some("b value"), Some(3.0)),
          Foo(2, None, Some(4.0)),
          Foo(3, Some("It's three"), None),
          Foo(4, None, None),
        ),
      )
      val bar2 = Foos(List())
      val xml1 = XmlEncoder[Foos].encode(bar1)
      val xml2 = XmlEncoder[Foos].encode(bar2)
      assert(
        xml1 ==
          """
            | <?xml version='1.0' encoding='UTF-8'?>
            | <foos>
            |   <foo b="b value">
            |     <a>1</a>
            |     <c>3.0</c>
            |   </foo>
            |   <foo>
            |     <a>2</a>
            |     <c>4.0</c>
            |   </foo>
            |   <foo b="It&apos;s three">
            |     <a>3</a>
            |   </foo>
            |   <foo>
            |     <a>4</a>
            |   </foo>
            | </foos>
          """.stripMargin.minimized
          && xml2 ==
          """
            | <?xml version='1.0' encoding='UTF-8'?>
            | <foos/>
          """.stripMargin.minimized,
      )
    }

    "encode byte arrays" in {
      @XmlCodec("foo")
      case class Foo(@text content: Array[Byte])

      val foo    = Foo("foobar".getBytes)
      val string = XmlEncoder[Foo].encode(foo)
      assert(
        string ==
          """<?xml version='1.0' encoding='UTF-8'?>
            | <foo>Zm9vYmFy</foo>
          """.stripMargin.minimized,
      )
    }

    "encode text values" in {
      @ElementCodec
      case class Foo(@attr a: Int, @attr b: String, @text c: Double)
      @XmlCodec("bar")
      case class Bar(d: String, foo: Foo, e: Char)

      val bar    = Bar("d value", Foo(1, "b value", 3.0), 'e')
      val string = XmlEncoder[Bar].encode(bar)
      assert(
        string ==
          """
            | <?xml version='1.0' encoding='UTF-8'?>
            | <bar>
            |   <d>d value</d>
            |   <foo a="1" b="b value">3.0</foo>
            |   <e>e</e>
            | </bar>
          """.stripMargin.minimized,
      )
    }

    "encode recursive values" in {
      @XmlCodec("foo")
      case class Foo(foo: Option[Foo], das: Int)

      val foo = Foo(Some(Foo(Some(Foo(Some(Foo(Some(Foo(None, 4)), 3)), 2)), 1)), 0)

      val string = XmlEncoder[Foo].encode(foo)
      assert(
        string ==
          """
            | <?xml version='1.0' encoding='UTF-8'?>
            | <foo>
            |    <foo>
            |        <foo>
            |            <foo>
            |               <foo>
            |                 <das>4</das>
            |               </foo>
            |               <das>3</das>
            |            </foo>
            |            <das>2</das>
            |        </foo>
            |        <das>1</das>
            |    </foo>
            |    <das>0</das>
            | </foo>
          """.stripMargin.minimized,
      )
    }

    "encode mixed content" in {
      @XmlCodec("foo")
      case class Foo(count: Int, buz: String, @text text: String)

      val foo    = Foo(1, "Buzz", "Sending item to ")
      val string = XmlEncoder[Foo].encode(foo)
      assert(
        string ==
          """
            | <?xml version='1.0' encoding='UTF-8'?>
            | <foo>Sending item to <count>1</count><buz>Buzz</buz></foo>
          """.stripMargin.minimized,
      )
    }

    "escape characters" in {
      @XmlCodec("foo")
      case class Foo(@attr baz: String, bar: String)

      val foo    = Foo("Esca\"'<>&pe", "Esca\"'<>&pe")
      val string = XmlEncoder[Foo].encode(foo)
      assert(
        string ==
          """
             | <?xml version='1.0' encoding='UTF-8'?>
             | <foo baz="Esca&quot;&apos;&lt;>&amp;pe">
             |   <bar>Esca"'&lt;>&amp;pe</bar>
             | </foo>
          """.stripMargin.minimized,
      )
    }

    "encode with @renamed" in {
      @ElementCodec
      case class Foo(a: Int, @renamed("theB") b: String, c: Double)
      @XmlCodec("bar")
      case class Bar(d: String, @renamed("theFoo") foo: Foo, e: Char)

      val bar    = Bar("d value", Foo(1, "b value", 3.0), 'e')
      val string = XmlEncoder[Bar].encode(bar)
      assert(
        string ==
          """
            | <?xml version='1.0' encoding='UTF-8'?>
            | <bar>
            |   <d>d value</d>
            |   <theFoo>
            |     <a>1</a>
            |     <theB>b value</theB>
            |     <c>3.0</c>
            |   </theFoo>
            |   <e>e</e>
            | </bar>
          """.stripMargin.minimized,
      )
    }

    "encode with @renamed @attr" in {
      @XmlCodec("foo")
      case class Foo(a: Int, @renamed("theB") @attr b: String, c: Double)

      val bar    = Foo(1, "b value", 3.0)
      val string = XmlEncoder[Foo].encode(bar)
      assert(
        string ==
          """
            | <?xml version='1.0' encoding='UTF-8'?>
            |   <foo theB="b value">
            |     <a>1</a>
            |     <c>3.0</c>
            |   </foo>
          """.stripMargin.minimized,
      )
    }

    "encode @renamed text values" in {
      @ElementCodec
      case class Foo(@attr a: Int, @attr @renamed("theB") b: String, @text c: Double)
      @XmlCodec("bar")
      case class Bar(d: String, foo: Foo, e: Char)

      val bar    = Bar("d value", Foo(1, "b value", 3.0), 'e')
      val string = XmlEncoder[Bar].encode(bar)
      assert(
        string ==
          """
            | <?xml version='1.0' encoding='UTF-8'?>
            | <bar>
            |   <d>d value</d>
            |   <foo a="1" theB="b value">3.0</foo>
            |   <e>e</e>
            | </bar>
          """.stripMargin.minimized,
      )
    }

    "encode CamelCase" in {
      val camelCaseConfig = ElementCodecConfig.default.withStyle(camelCase)
      @ElementCodec(camelCaseConfig)
      case class Foo(@attr someName: Int, @attr someOther: String, @text c: Double)
      @XmlCodec("Bar", camelCaseConfig)
      case class Bar(someTopName: String, someFoo: Foo, e: Char)

      val bar    = Bar("d value", Foo(1, "b value", 3.0), 'e')
      val string = XmlEncoder[Bar].encode(bar)
      assert(
        string ==
          """
            |<?xml version='1.0' encoding='UTF-8'?>
            | <Bar>
            |   <SomeTopName>d value</SomeTopName>
            |   <SomeFoo SomeName="1" SomeOther="b value">3.0</SomeFoo>
            |   <E>e</E>
            | </Bar>
          """.stripMargin.minimized,
      )
    }

    "encode snake_case" in {
      val snakeCaseConfig = ElementCodecConfig.default.withStyle(snakeCase)
      @ElementCodec(snakeCaseConfig)
      case class Foo(@attr someName: Int, @attr someOther: String, @text c: Double)
      @XmlCodec("bar", snakeCaseConfig)
      case class Bar(someTopName: String, someFoo: Foo, e: Char)

      val bar    = Bar("d value", Foo(1, "b value", 3.0), 'e')
      val string = XmlEncoder[Bar].encode(bar)
      assert(
        string ==
          """
            |<?xml version='1.0' encoding='UTF-8'?>
            | <bar>
            |   <some_top_name>d value</some_top_name>
            |   <some_foo some_name="1" some_other="b value">3.0</some_foo>
            |   <e>e</e>
            | </bar>
          """.stripMargin.minimized,
      )
    }
  }

  "Encoder derivation for sealed traits" should {
    "encode simple sealed traits" in {
      @ElementCodec
      case class Bar(d: String, foo: SealedClasses.Foo, e: Char)

      val bar1 = Bar("d value", SealedClasses.Foo1("string"), 'k')
      val bar2 = Bar("d value", SealedClasses.Foo2(1), 'e')
      val bar3 = Bar("another one value", SealedClasses.Foo3(1.1234), 'v')

      val barEncoder = XmlEncoder.fromElementEncoder[Bar]("bar")

      val string1 = barEncoder.encode(bar1)
      val string2 = barEncoder.encode(bar2)
      val string3 = barEncoder.encode(bar3)

      assert(
        string1 ==
          """
            | <?xml version='1.0' encoding='UTF-8'?>
            | <bar>
            |   <d>d value</d>
            |   <foo xmlns:ans1="http://www.w3.org/2001/XMLSchema-instance" ans1:type="Foo1">
            |     <a>string</a>
            |   </foo>
            |   <e>k</e>
            | </bar>
          """.stripMargin.minimized &&
          string2 ==
          """
              | <?xml version='1.0' encoding='UTF-8'?>
              | <bar>
              |   <d>d value</d>
              |   <foo xmlns:ans1="http://www.w3.org/2001/XMLSchema-instance" ans1:type="Foo2">
              |     <b>1</b>
              |   </foo>
              |   <e>e</e>
              | </bar>
          """.stripMargin.minimized &&
          string3 ==
          """
              | <?xml version='1.0' encoding='UTF-8'?>
              | <bar>
              |   <d>another one value</d>
              |   <foo xmlns:ans1="http://www.w3.org/2001/XMLSchema-instance" ans1:type="Foo3">
              |     <c>1.1234</c>
              |   </foo>
              |   <e>v</e>
              | </bar>
          """.stripMargin.minimized,
      )
    }

    "encode sealed traits with custom discriminator" in {
      @ElementCodec
      case class Qux(d: String, bar: SealedClasses.Bar, e: Char)

      val qux1 = Qux("d value", SealedClasses.Bar1("string"), 'k')
      val qux2 = Qux("d value", SealedClasses.Bar2(1), 'e')
      val qux3 = Qux("another one value", SealedClasses.Bar3(1.1234), 'v')

      val quxEncoder = XmlEncoder.fromElementEncoder[Qux]("qux")

      val string1 = quxEncoder.encode(qux1)
      val string2 = quxEncoder.encode(qux2)
      val string3 = quxEncoder.encode(qux3)

      assert(
        string1 ==
          """
            | <?xml version='1.0' encoding='UTF-8'?>
            | <qux>
            |   <d>d value</d>
            |   <bar discriminator="Bar1">
            |     <a>string</a>
            |   </bar>
            |   <e>k</e>
            | </qux>
          """.stripMargin.minimized &&
          string2 ==
          """
              | <?xml version='1.0' encoding='UTF-8'?>
              | <qux>
              |   <d>d value</d>
              |   <bar discriminator="Bar2">
              |     <b>1</b>
              |   </bar>
              |   <e>e</e>
              | </qux>
          """.stripMargin.minimized &&
          string3 ==
          """
              | <?xml version='1.0' encoding='UTF-8'?>
              | <qux>
              |   <d>another one value</d>
              |   <bar discriminator="Bar3">
              |     <c>1.1234</c>
              |   </bar>
              |   <e>v</e>
              | </qux>
          """.stripMargin.minimized,
      )

      @ElementCodec
      case class Quux(d: String, baz: SealedClasses.Baz, e: Char)

      val quux1 = Quux("d value", SealedClasses.Baz1("string"), 'k')
      val quux2 = Quux("d value", SealedClasses.Baz2(1), 'e')
      val quux3 = Quux("another one value", SealedClasses.Baz3(1.1234), 'v')

      val quuxEncoder = XmlEncoder.fromElementEncoder[Quux]("quux")

      val string4 = quuxEncoder.encode(quux1)
      val string5 = quuxEncoder.encode(quux2)
      val string6 = quuxEncoder.encode(quux3)

      assert(
        string4 ==
          """
            | <?xml version='1.0' encoding='UTF-8'?>
            | <quux>
            |   <d>d value</d>
            |   <baz xmlns:ans1="https://tinkoff.ru" ans1:discriminator="Baz1">
            |     <a>string</a>
            |   </baz>
            |   <e>k</e>
            | </quux>
          """.stripMargin.minimized &&
          string5 ==
          """
              | <?xml version='1.0' encoding='UTF-8'?>
              | <quux>
              |   <d>d value</d>
              |   <baz xmlns:ans1="https://tinkoff.ru" ans1:discriminator="Baz2">
              |     <b>1</b>
              |   </baz>
              |   <e>e</e>
              | </quux>
          """.stripMargin.minimized &&
          string6 ==
          """
              | <?xml version='1.0' encoding='UTF-8'?>
              | <quux>
              |   <d>another one value</d>
              |   <baz xmlns:ans1="https://tinkoff.ru" ans1:discriminator="Baz3">
              |     <c>1.1234</c>
              |   </baz>
              |   <e>v</e>
              | </quux>
          """.stripMargin.minimized,
      )
    }

    "encode sealed traits with constructor names transformed" in {
      val wolf = SealedClasses.CanisLupus("Igor", 0.2, 20)
      val lion = SealedClasses.PantheraLeo("Sergey", 0.75, 60.1)

      val animalEncoder = XmlEncoder.fromElementEncoder[SealedClasses.Mammalia]("animal")

      assert(
        animalEncoder.encode(wolf) ==
          """
            | <?xml version='1.0' encoding='UTF-8'?>
            | <animal xmlns:ans1="http://www.w3.org/2001/XMLSchema-instance" ans1:type="canis_lupus">
            |   <name>Igor</name>
            |   <strength>0.2</strength>
            |   <age>20</age>
            | </animal>
          """.stripMargin.minimized &&
          animalEncoder.encode(lion) ==
          """
              | <?xml version='1.0' encoding='UTF-8'?>
              | <animal xmlns:ans1="http://www.w3.org/2001/XMLSchema-instance" ans1:type="panthera_leo">
              |   <name>Sergey</name>
              |   <strength>0.75</strength>
              |   <speed>60.1</speed>
              | </animal>
            """.stripMargin.minimized,
      )
    }

    "encode sealed traits with custom discriminator values" in {
      val hornet    = SealedClasses.Vespa("Anton", 200.123)
      val cockroach = SealedClasses.Blattodea("Dmitriy", 5)

      val insectEncoder = XmlEncoder.fromElementEncoder[SealedClasses.Insecta]("insect")

      assert(
        insectEncoder.encode(hornet) ==
          """
            | <?xml version='1.0' encoding='UTF-8'?>
            | <insect xmlns:ans1="http://www.w3.org/2001/XMLSchema-instance" ans1:type="hornet">
            |   <name>Anton</name>
            |   <damage>200.123</damage>
            | </insect>
          """.stripMargin.minimized &&
          insectEncoder.encode(cockroach) ==
          """
              | <?xml version='1.0' encoding='UTF-8'?>
              | <insect xmlns:ans1="http://www.w3.org/2001/XMLSchema-instance" ans1:type="cockroach">
              |   <name>Dmitriy</name>
              |   <legsNumber>5</legsNumber>
              | </insect>
            """.stripMargin.minimized,
      )
    }

    "not transform custom discriminator values" in {
      val clownFish  = SealedClasses.Amphiprion("Nemo", 1)
      val whiteShark = SealedClasses.CarcharodonCarcharias("Bill", 20000000000L)

      val fishEncoder = XmlEncoder.fromElementEncoder[SealedClasses.Pisces]("fish")

      assert(
        fishEncoder.encode(clownFish) ==
          """
            | <?xml version='1.0' encoding='UTF-8'?>
            | <fish xmlns:ans1="http://www.w3.org/2001/XMLSchema-instance" ans1:type="ClownFish">
            |   <name>Nemo</name>
            |   <finNumber>1</finNumber>
            | </fish>
          """.stripMargin.minimized &&
          fishEncoder.encode(whiteShark) ==
          """
              | <?xml version='1.0' encoding='UTF-8'?>
              | <fish xmlns:ans1="http://www.w3.org/2001/XMLSchema-instance" ans1:type="carcharodon_carcharias">
              |   <name>Bill</name>
              |   <teethNumber>20000000000</teethNumber>
              | </fish>
            """.stripMargin.minimized,
      )
    }

    "use element name as discriminator if configured" in {
      @XmlCodec("zoo")
      case class Zoo(@default animals: List[Animal])
      val string =
        """<?xml version='1.0' encoding='UTF-8'?>
          | <zoo>
          |   <cow>
          |     <moo>12.432</moo>
          |   </cow>
          |   <cat>
          |     <meow>meow</meow>
          |   </cat>
          |   <dog>
          |     <woof>1234</woof>
          |   </dog>
          |   <cat>
          |     <meow>nya</meow>
          |   </cat>
          | </zoo>
                    """.stripMargin.minimized
      val zoo = Zoo(List(Cow(12.432), Cat("meow"), Dog(1234), Cat("nya")))
      XmlEncoder[Zoo].encode(zoo) shouldBe string
    }

    "override element name with discriminator in xml encoder if configured" in {
      // "animal" must be ignored if useElementNamesAsDiscriminator is set to true
      val animalXmlEncoder: XmlEncoder[Animal] = XmlEncoder.fromElementEncoder("animal")
      val cat                                  = Cat("meow")
      val catString =
        """<?xml version='1.0' encoding='UTF-8'?>
          | <cat>
          |  <meow>meow</meow>
          | </cat>
          |""".stripMargin.minimized
      val dog = Dog(1234)
      val dogString =
        """<?xml version='1.0' encoding='UTF-8'?>
          | <dog>
          |  <woof>1234</woof>
          | </dog>
          |""".stripMargin.minimized
      animalXmlEncoder.encode(cat) shouldBe catString
      animalXmlEncoder.encode(dog) shouldBe dogString
    }
  }

  "Encoder derivation with namespaces" should {
    "encode simple case classes" in {
      @XmlnsDef("tinkoff.ru")
      case object tkf
      @ElementCodec
      case class Foo(
          @xmlns(tkf) a: Int,
          @xmlns(tkf) b: String,
          @xmlns(tkf) c: Double,
      )
      @XmlCodecNs("bar", tkf)
      case class Bar(
          @xmlns(tkf) d: String,
          @xmlns(tkf) foo: Foo,
          @xmlns(tkf) e: Char,
      )

      val bar    = Bar("d value", Foo(1, "b value", 3.0), 'e')
      val string = XmlEncoder[Bar].encode(bar)
      assert(
        string ==
          """
            | <?xml version='1.0' encoding='UTF-8'?>
            | <ans1:bar xmlns:ans1="tinkoff.ru">
            |   <ans1:d>d value</ans1:d>
            |   <ans1:foo>
            |     <ans1:a>1</ans1:a>
            |     <ans1:b>b value</ans1:b>
            |     <ans1:c>3.0</ans1:c>
            |   </ans1:foo>
            |   <ans1:e>e</ans1:e>
            | </ans1:bar>
          """.stripMargin.minimized,
      )
    }

    "encode attributes" in {
      @XmlnsDef("tinkoff.ru")
      case object tkf
      @ElementCodec
      case class Foo(
          @xmlns(tkf) a: Int,
          @xmlns(tkf) @attr b: String,
          @xmlns(tkf) c: Double,
      )
      @XmlCodecNs("bar", tkf)
      case class Bar(
          @xmlns(tkf) d: String,
          @xmlns(tkf) foo: Foo,
          @xmlns(tkf) @attr e: Char,
      )

      val bar    = Bar("d value", Foo(1, "b value", 3.0), 'e')
      val string = XmlEncoder[Bar].encode(bar)
      assert(
        string ==
          """
            | <?xml version='1.0' encoding='UTF-8'?>
            | <ans1:bar xmlns:ans1="tinkoff.ru" ans1:e="e">
            |   <ans1:d>d value</ans1:d>
            |   <ans1:foo ans1:b="b value">
            |     <ans1:a>1</ans1:a>
            |     <ans1:c>3.0</ans1:c>
            |   </ans1:foo>
            | </ans1:bar>
          """.stripMargin.minimized,
      )
    }

    "encode nested namespace" in {
      @XmlnsDef("tinkoff.ru")
      case object tkf
      @ElementCodec
      case class Foo(
          @xmlns(tkf) a: Int,
          @attr b: String,
          @xmlns(tkf) c: Double,
      )
      @XmlCodec("bar")
      case class Bar(
          d: String,
          @xmlns(tkf) foo: Foo,
          @attr e: Char,
      )

      val bar    = Bar("d value", Foo(1, "b value", 3.0), 'e')
      val string = XmlEncoder[Bar].encode(bar)
      assert(
        string ==
          """
            | <?xml version='1.0' encoding='UTF-8'?>
            | <bar e="e">
            |   <d>d value</d>
            |   <ans1:foo xmlns:ans1="tinkoff.ru" b="b value">
            |     <ans1:a>1</ans1:a>
            |     <ans1:c>3.0</ans1:c>
            |   </ans1:foo>
            | </bar>
          """.stripMargin.minimized,
      )
    }

    "encode multiple namespaces" in {
      @XmlnsDef("tinkoff.ru")
      case object tkf
      @XmlnsDef("tcsbank.ru")
      case object tcs
      @ElementCodec
      case class Foo(
          @xmlns(tkf) a: Int,
          @attr b: String,
          @xmlns(tkf) c: Double,
      )
      @XmlCodecNs("bar", tcs)
      case class Bar(
          @xmlns(tcs) d: String,
          @xmlns(tkf) foo: Foo,
          @attr e: Char,
      )

      val bar    = Bar("d value", Foo(1, "b value", 3.0), 'e')
      val string = XmlEncoder[Bar].encode(bar)
      assert(
        string ==
          """
            | <?xml version='1.0' encoding='UTF-8'?>
            | <ans1:bar xmlns:ans1="tcsbank.ru" e="e">
            |   <ans1:d>d value</ans1:d>
            |   <ans2:foo xmlns:ans2="tinkoff.ru" b="b value">
            |     <ans2:a>1</ans2:a>
            |     <ans2:c>3.0</ans2:c>
            |   </ans2:foo>
            | </ans1:bar>
          """.stripMargin.minimized,
      )
    }

    "encode CamelCase" in {
      val camelCaseConfig = ElementCodecConfig.default.withStyle(camelCase)
      @XmlnsDef("tinkoff.ru")
      case object tkf
      @ElementCodec(camelCaseConfig)
      case class Foo(
          @xmlns(tkf) someName: Int,
          @xmlns(tkf) someOtherName: String,
          @xmlns(tkf) c: Double,
      )
      @XmlCodecNs("Bar", tkf, camelCaseConfig)
      case class Bar(
          @xmlns(tkf) someTopName: String,
          @xmlns(tkf) someFoo: Foo,
      )

      val bar    = Bar("d value", Foo(1, "b value", 3.0))
      val string = XmlEncoder[Bar].encode(bar)
      assert(
        string ==
          """
            | <?xml version='1.0' encoding='UTF-8'?>
            | <ans1:Bar xmlns:ans1="tinkoff.ru">
            |   <ans1:SomeTopName>d value</ans1:SomeTopName>
            |   <ans1:SomeFoo>
            |     <ans1:SomeName>1</ans1:SomeName>
            |     <ans1:SomeOtherName>b value</ans1:SomeOtherName>
            |     <ans1:C>3.0</ans1:C>
            |   </ans1:SomeFoo>
            | </ans1:Bar>
          """.stripMargin.minimized,
      )
    }

    "encode snake_case" in {
      val snakeCaseConfig = ElementCodecConfig.default.withStyle(snakeCase)
      @XmlnsDef("tinkoff.ru")
      case object tkf
      @ElementCodec(snakeCaseConfig)
      case class Foo(
          @xmlns(tkf) someName: Int,
          @xmlns(tkf) someOtherName: String,
          @xmlns(tkf) c: Double,
      )
      @XmlCodecNs("bar", tkf, snakeCaseConfig)
      case class Bar(
          @xmlns(tkf) someTopName: String,
          @xmlns(tkf) someFoo: Foo,
      )

      val bar    = Bar("d value", Foo(1, "b value", 3.0))
      val string = XmlEncoder[Bar].encode(bar)
      assert(
        string ==
          """
            | <?xml version='1.0' encoding='UTF-8'?>
            | <ans1:bar xmlns:ans1="tinkoff.ru">
            |   <ans1:some_top_name>d value</ans1:some_top_name>
            |   <ans1:some_foo>
            |     <ans1:some_name>1</ans1:some_name>
            |     <ans1:some_other_name>b value</ans1:some_other_name>
            |     <ans1:c>3.0</ans1:c>
            |   </ans1:some_foo>
            | </ans1:bar>
          """.stripMargin.minimized,
      )
    }

    "encode with @renamed having priority over naming" in {
      val snakeCaseConfig = ElementCodecConfig.default.withStyle(snakeCase)
      @ElementCodec(snakeCaseConfig)
      case class Foo(@attr someName: Int, @attr @renamed("i-Have-priority") someOther: String, @text c: Double)
      @XmlCodec("bar", snakeCaseConfig)
      case class Bar(someTopName: String, @renamed("Me2") someFoo: Foo, e: Char)

      val bar = Bar("d value", Foo(1, "b value", 3.0), 'e')

      val string = XmlEncoder[Bar].encode(bar)
      assert(
        string ==
          """<?xml version='1.0' encoding='UTF-8'?>
            | <bar>
            |   <some_top_name>d value</some_top_name>
            |   <Me2 some_name="1" i-Have-priority="b value">3.0</Me2>
            |   <e>e</e>
            | </bar>
          """.stripMargin.minimized,
      )
    }

    "encode with default element namespaces" in {
      @XmlnsDef("tinkoff.ru")
      case object tkf

      val defaultNamespaceConfig = ElementCodecConfig.default.withElementsDefaultNamespace(tkf)
      @ElementCodec
      case class Foo(a: Int, b: String, c: Double)
      @XmlCodecNs("bar", tkf, defaultNamespaceConfig)
      case class Bar(@attr d: String, foo: Foo, e: Char)

      val bar    = Bar("d value", Foo(1, "b value", 3.0), 'e')
      val string = XmlEncoder[Bar].encode(bar)

      assert(
        string ==
          """<?xml version='1.0' encoding='UTF-8'?>
          | <ans1:bar xmlns:ans1="tinkoff.ru" d="d value">
          |   <ans1:foo>
          |     <a>1</a>
          |     <b>b value</b>
          |     <c>3.0</c>
          |   </ans1:foo>
          |  <ans1:e>e</ans1:e>
          | </ans1:bar>
      """.stripMargin.minimized,
      )
    }

    "override default element namespace with namespace from annotation" in {
      @XmlnsDef("tinkoff.ru")
      case object tkf

      @XmlnsDef("tcsbank.ru")
      case object tcs

      val defaultNamespaceConfig = ElementCodecConfig.default.withElementsDefaultNamespace(tkf)
      @ElementCodec
      case class Foo(a: Int, b: String, c: Double)
      @XmlCodecNs("bar", tkf, defaultNamespaceConfig)
      case class Bar(@attr d: String, @xmlns(tcs) foo: Foo, e: Char)

      val bar    = Bar("d value", Foo(1, "b value", 3.0), 'e')
      val string = XmlEncoder[Bar].encode(bar)

      assert(
        string ==
          """<?xml version='1.0' encoding='UTF-8'?>
            | <ans1:bar xmlns:ans1="tinkoff.ru" d="d value">
            |   <ans2:foo xmlns:ans2="tcsbank.ru">
            |     <a>1</a>
            |     <b>b value</b>
            |     <c>3.0</c>
            |   </ans2:foo>
            |  <ans1:e>e</ans1:e>
            | </ans1:bar>
      """.stripMargin.minimized,
      )
    }

    "encode with default attribute namespaces" in {
      @XmlnsDef("tinkoff.ru")
      case object tkf

      val defaultNamespaceConfig = ElementCodecConfig.default.withAttributesDefaultNamespace(tkf)
      @ElementCodec
      case class Foo(a: Int, b: String, c: Double)
      @XmlCodecNs("bar", tkf, defaultNamespaceConfig)
      case class Bar(@attr d: String, foo: Foo, @attr e: Char)

      val bar    = Bar("d value", Foo(1, "b value", 3.0), 'e')
      val string = XmlEncoder[Bar].encode(bar)

      assert(
        string ==
          """<?xml version='1.0' encoding='UTF-8'?>
            | <ans1:bar xmlns:ans1="tinkoff.ru" ans1:d="d value" ans1:e="e">
            |   <foo>
            |     <a>1</a>
            |     <b>b value</b>
            |     <c>3.0</c>
            |   </foo>
            | </ans1:bar>
      """.stripMargin.minimized,
      )
    }

    "override default attribute namespace with namespace from annotation" in {
      @XmlnsDef("tinkoff.ru")
      case object tkf

      @XmlnsDef("tcsbank.ru")
      case object tcs

      val defaultNamespaceConfig = ElementCodecConfig.default.withAttributesDefaultNamespace(tkf)
      @ElementCodec
      case class Foo(a: Int, b: String, c: Double)
      @XmlCodecNs("bar", tkf, defaultNamespaceConfig)
      case class Bar(@attr d: String, foo: Foo, @attr @xmlns(tcs) e: Char)

      val bar    = Bar("d value", Foo(1, "b value", 3.0), 'e')
      val string = XmlEncoder[Bar].encode(bar)

      assert(
        string ==
          """<?xml version='1.0' encoding='UTF-8'?>
            | <ans1:bar xmlns:ans1="tinkoff.ru" ans1:d="d value" xmlns:ans2="tcsbank.ru" ans2:e="e">
            |   <foo>
            |     <a>1</a>
            |     <b>b value</b>
            |     <c>3.0</c>
            |   </foo>
            | </ans1:bar>
      """.stripMargin.minimized,
      )
    }

    "define namespaces from config" in {
      @XmlnsDef("tinkoff.ru")
      case object tkf

      @XmlnsDef("tcsbank.ru")
      case object tcs

      val config = ElementCodecConfig.default.withNamespaceDefined(tkf).withNamespaceDefined(tcs)

      @XmlCodec("foo", config)
      final case class Foo(
          @xmlns(tcs) a: Int,
          @xmlns(tcs) b: String,
          @xmlns(tcs) c: Double,
      )

      val foo    = Foo(1, "b value", 3.0)
      val string = XmlEncoder[Foo].encode(foo)

      assert(
        string ==
          """<?xml version='1.0' encoding='UTF-8'?>
            | <foo xmlns:ans1="tcsbank.ru" xmlns:ans2="tinkoff.ru">
            |   <ans1:a>1</ans1:a>
            |   <ans1:b>b value</ans1:b>
            |   <ans1:c>3.0</ans1:c>
            | </foo>
      """.stripMargin.minimized,
      )
    }

    "define namespaces with not bounded prefixes" in {
      @XmlnsDef("tinkoff.ru")
      case object tkf

      @XmlnsDef("tcsbank.ru")
      case object tcs

      val config =
        List
          .range(10, 2, -1)
          .foldLeft(ElementCodecConfig.default)((config, i) => config.withNamespaceDefined(s"example.com/$i"))

      @ElementCodec(config)
      final case class Foo(
          a: Int,
          b: String,
          c: Double,
      )

      @XmlCodecNs("bar", tkf)
      final case class Bar(
          @xmlns(tcs) foo: Foo,
      )

      val bar    = Bar(Foo(1, "b value", 3.0))
      val string = XmlEncoder[Bar].encode(bar)
      // \u0020 - space, for .minimize to work correctly
      assert(
        string ==
          """<?xml version='1.0' encoding='UTF-8'?>
          | <ans1:bar xmlns:ans1="tinkoff.ru">
          |   <ans2:foo xmlns:ans2="tcsbank.ru" xmlns:ans3="example.com/3" xmlns:ans4="example.com/4"\u0020
          |             xmlns:ans5="example.com/5" xmlns:ans6="example.com/6" xmlns:ans7="example.com/7"\u0020
          |             xmlns:ans8="example.com/8" xmlns:ans9="example.com/9" xmlns:ans10="example.com/10">
          |     <a>1</a>
          |     <b>b value</b>
          |     <c>3.0</c>
          |   </ans2:foo>
          | </ans1:bar>
      """.stripMargin.minimized,
      )
    }

    "not define already defined namespaces" in {
      @XmlnsDef("tinkoff.ru")
      case object tkf

      @XmlnsDef("tcsbank.ru")
      case object tcs

      val config = ElementCodecConfig.default.withNamespaceDefined(tkf).withNamespaceDefined(tcs)
      @ElementCodec(config)
      final case class Foo(
          a: Int,
          b: String,
          c: Double,
      )

      @XmlCodecNs("bar", tkf)
      final case class Bar(
          foo: Foo,
      )

      val bar    = Bar(Foo(1, "b value", 3.0))
      val string = XmlEncoder[Bar].encode(bar)
      assert(
        string ==
          """<?xml version='1.0' encoding='UTF-8'?>
            | <ans1:bar xmlns:ans1="tinkoff.ru">
            |   <foo xmlns:ans2="tcsbank.ru">
            |     <a>1</a>
            |     <b>b value</b>
            |     <c>3.0</c>
            |   </foo>
            | </ans1:bar>
      """.stripMargin.minimized,
      )
    }
  }

  "Encoder derivation compilation" should {
    "fail if wrong attributes" in {
      """
        | @ElementCodec
        | case class NotAttribute(a: Int)
        | @ElementCodec
        | case class Wrapper(@attr attribute: NotAttribute)
      """.stripMargin shouldNot typeCheck
    }

    "fail if wrong text" in {
      """
        | @ElementCodec
        | case class NotText(a: Int)
        | @ElementCodec
        | case class Wrapper(@text text: NotText, @attr a: Int)
      """.stripMargin shouldNot typeCheck
    }

    "fail if multiple xml annotations" in {
      """
        | @ElementCodec
        | case class AttrText(@attr @text attrText: Int, b: String)
      """.stripMargin shouldNot typeCheck
    }

    "fail if multiple texts" in {
      """
        | @ElementCodec
        | case class MultiText(@text a: Int, @text b: String)
      """.stripMargin shouldNot typeCheck
    }
  }
}
