package ru.tinkoff.phobos

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import ru.tinkoff.phobos.SealedClasses.{Animal, Cat, Cow, Dog}
import ru.tinkoff.phobos.encoding.{AttributeEncoder, ElementEncoder, TextEncoder, XmlEncoder}
import ru.tinkoff.phobos.testString._
import ru.tinkoff.phobos.syntax._
import ru.tinkoff.phobos.configured.naming._
import ru.tinkoff.phobos.configured.ElementCodecConfig
import ru.tinkoff.phobos.derivation.semiauto._

import scala.annotation.nowarn

@nowarn("msg=is never used")
class EncoderDerivationTest extends AnyWordSpec with Matchers {

  "Encoder derivation without namespaces" should {
    "encode simple case classes" in {
      case class Foo(a: Int, b: String, c: Double)
      case class Bar(d: String, foo: Foo, e: Char)

      implicit val fooEncoder: ElementEncoder[Foo] = deriveElementEncoder
      implicit val xmlEncoder: XmlEncoder[Bar]     = deriveXmlEncoder("bar")

      val bar    = Bar("d value", Foo(1, "b value", 3.0), 'e')
      val string = xmlEncoder.encode(bar)
      assert(
        string ==
          Right("""
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
          """.stripMargin.minimized),
      )
    }

    "encode attributes" in {
      case class Foo(a: Int, @attr b: String, c: Double)
      case class Bar(d: String, foo: Foo, @attr e: Char)

      implicit val fooEncoder: ElementEncoder[Foo] = deriveElementEncoder
      implicit val xmlEncoder: XmlEncoder[Bar]     = deriveXmlEncoder("bar")

      val bar    = Bar("d value", Foo(1, "b value", 3.0), 'e')
      val string = XmlEncoder[Bar].encode(bar)
      assert(
        string ==
          Right("""
            | <?xml version='1.0' encoding='UTF-8'?>
            | <bar e="e">
            |   <d>d value</d>
            |   <foo b="b value">
            |     <a>1</a>
            |     <c>3.0</c>
            |   </foo>
            | </bar>
          """.stripMargin.minimized),
      )
    }

    "allow to override codecs" in {
      implicit val alternativeElementEncoder: ElementEncoder[String] =
        ElementEncoder.stringEncoder.contramap(_ => "constant")
      implicit val alternativeAttributeEncoder: AttributeEncoder[Int] =
        AttributeEncoder.stringEncoder.contramap(_ => "a74153b")
      implicit val alternativeTextEncoder: TextEncoder[Double] =
        TextEncoder.stringEncoder.contramap(_ => "text")

      case class Foo(@attr bar: Int, @text baz: Double)
      implicit val fooEncoder: ElementEncoder[Foo] = deriveElementEncoder
      case class Qux(str: String, foo: Foo)
      implicit val quxEncoder: XmlEncoder[Qux] = deriveXmlEncoder[Qux]("qux")

      val qux = Qux("42", Foo(42, 12.2))
      val xml = XmlEncoder[Qux].encode(qux)
      assert(
        xml ==
          Right("""
          | <?xml version='1.0' encoding='UTF-8'?>
          | <qux>
          |   <str>constant</str>
          |   <foo bar="a74153b">text</foo>
          | </qux>
          """.stripMargin.minimized),
      )
    }

    "encode options" in {
      case class Foo(a: Int, @attr b: String, c: Double)
      case class Wrapper(foo: Option[Foo])

      implicit val fooEncoder: ElementEncoder[Foo] = deriveElementEncoder
      implicit val xmlEncoder: XmlEncoder[Wrapper] = deriveXmlEncoder("Wrapper")

      val xml1 = XmlEncoder[Wrapper].encode(Wrapper(Some(Foo(1, "b", 2.0))))
      val xml2 = XmlEncoder[Wrapper].encode(Wrapper(None))
      assert(
        xml1 ==
          Right("""
            | <?xml version='1.0' encoding='UTF-8'?>
            | <Wrapper>
            |   <foo b="b">
            |     <a>1</a>
            |     <c>2.0</c>
            |   </foo>
            | </Wrapper>
          """.stripMargin.minimized) &&
          xml2 ==
          Right("""
              | <?xml version='1.0' encoding='UTF-8'?>
              | <Wrapper/>
            """.stripMargin.minimized),
      )
    }

    "encode lists" in {
      case class Foo(a: Int, @attr b: Option[String], c: Option[Double])
      case class Foos(foo: List[Foo])

      implicit val fooEncoder: ElementEncoder[Foo] = deriveElementEncoder
      implicit val xmlEncoder: XmlEncoder[Foos]    = deriveXmlEncoder("foos")

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
          Right("""
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
          """.stripMargin.minimized)
          && xml2 ==
          Right("""
            | <?xml version='1.0' encoding='UTF-8'?>
            | <foos/>
          """.stripMargin.minimized),
      )
    }

    "encode byte arrays" in {
      case class Foo(@text content: Array[Byte])

      implicit val xmlEncoder: XmlEncoder[Foo] = deriveXmlEncoder("foo")

      val foo    = Foo("foobar".getBytes)
      val string = XmlEncoder[Foo].encode(foo)
      assert(
        string ==
          Right("""<?xml version='1.0' encoding='UTF-8'?>
            | <foo>Zm9vYmFy</foo>
          """.stripMargin.minimized),
      )
    }

    "encode text values" in {
      case class Foo(@attr a: Int, @attr b: String, @text c: Double)
      case class Bar(d: String, foo: Foo, e: Char)

      implicit val fooEncoder: ElementEncoder[Foo] = deriveElementEncoder
      implicit val xmlEncoder: XmlEncoder[Bar]     = deriveXmlEncoder("bar")

      val bar    = Bar("d value", Foo(1, "b value", 3.0), 'e')
      val string = XmlEncoder[Bar].encode(bar)
      assert(
        string ==
          Right("""
            | <?xml version='1.0' encoding='UTF-8'?>
            | <bar>
            |   <d>d value</d>
            |   <foo a="1" b="b value">3.0</foo>
            |   <e>e</e>
            | </bar>
          """.stripMargin.minimized),
      )
    }

    object encodeRecursiveValuesClasses {
      case class Foo(foo: Option[Foo], das: Int)

      implicit val fooEncoder: ElementEncoder[Foo] = deriveElementEncoder[Foo]
      implicit val xmlEncoder: XmlEncoder[Foo]     = XmlEncoder.fromElementEncoder("foo")
    }

    "encode recursive values" in {
      import encodeRecursiveValuesClasses._

      val foo = Foo(Some(Foo(Some(Foo(Some(Foo(Some(Foo(None, 4)), 3)), 2)), 1)), 0)

      val string = XmlEncoder[Foo].encode(foo)
      assert(
        string ==
          Right("""
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
          """.stripMargin.minimized),
      )
    }

    "encode mixed content" in {
      case class Foo(count: Int, buz: String, @text text: String)

      implicit val xmlEncoder: XmlEncoder[Foo] = deriveXmlEncoder("foo")

      val foo    = Foo(1, "Buzz", "Sending item to ")
      val string = XmlEncoder[Foo].encode(foo)
      assert(
        string ==
          Right("""
            | <?xml version='1.0' encoding='UTF-8'?>
            | <foo>Sending item to <count>1</count><buz>Buzz</buz></foo>
          """.stripMargin.minimized),
      )
    }

    "escape characters" in {
      case class Foo(@attr baz: String, bar: String)

      implicit val xmlEncoder: XmlEncoder[Foo] = deriveXmlEncoder("foo")

      val foo    = Foo("Esca\"'<>&pe", "Esca\"'<>&pe")
      val string = XmlEncoder[Foo].encode(foo)
      assert(
        string ==
          Right("""
             | <?xml version='1.0' encoding='UTF-8'?>
             | <foo baz="Esca&quot;&apos;&lt;>&amp;pe">
             |   <bar>Esca"'&lt;>&amp;pe</bar>
             | </foo>
          """.stripMargin.minimized),
      )
    }

    "encode with @renamed" in {
      case class Foo(a: Int, @renamed("theB") b: String, c: Double)
      case class Bar(d: String, @renamed("theFoo") foo: Foo, e: Char)

      implicit val fooEncoder: ElementEncoder[Foo] = deriveElementEncoder
      implicit val xmlEncoder: XmlEncoder[Bar]     = deriveXmlEncoder("bar")

      val bar    = Bar("d value", Foo(1, "b value", 3.0), 'e')
      val string = XmlEncoder[Bar].encode(bar)
      assert(
        string ==
          Right("""
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
          """.stripMargin.minimized),
      )
    }

    "encode with @renamed @attr" in {
      case class Foo(a: Int, @renamed("theB") @attr b: String, c: Double)

      implicit val xmlEncoder: XmlEncoder[Foo] = deriveXmlEncoder("foo")

      val bar    = Foo(1, "b value", 3.0)
      val string = XmlEncoder[Foo].encode(bar)
      assert(
        string ==
          Right("""
            | <?xml version='1.0' encoding='UTF-8'?>
            |   <foo theB="b value">
            |     <a>1</a>
            |     <c>3.0</c>
            |   </foo>
          """.stripMargin.minimized),
      )
    }

    "encode @renamed text values" in {
      case class Foo(@attr a: Int, @attr @renamed("theB") b: String, @text c: Double)
      case class Bar(d: String, foo: Foo, e: Char)

      implicit val fooEncoder: ElementEncoder[Foo] = deriveElementEncoder
      implicit val xmlEncoder: XmlEncoder[Bar]     = deriveXmlEncoder("bar")

      val bar    = Bar("d value", Foo(1, "b value", 3.0), 'e')
      val string = XmlEncoder[Bar].encode(bar)
      assert(
        string ==
          Right("""
            | <?xml version='1.0' encoding='UTF-8'?>
            | <bar>
            |   <d>d value</d>
            |   <foo a="1" theB="b value">3.0</foo>
            |   <e>e</e>
            | </bar>
          """.stripMargin.minimized),
      )
    }

    "encode CamelCase" in {
      val camelCaseConfig = ElementCodecConfig.default.withFieldNamesTransformed(camelCase)
      case class Foo(@attr someName: Int, @attr someOther: String, @text c: Double)
      case class Bar(someTopName: String, someFoo: Foo, e: Char)

      implicit val fooEncoder: ElementEncoder[Foo] = deriveElementEncoderConfigured(camelCaseConfig)
      implicit val xmlEncoder: XmlEncoder[Bar]     = deriveXmlEncoderConfigured("Bar", camelCaseConfig)

      val bar    = Bar("d value", Foo(1, "b value", 3.0), 'e')
      val string = XmlEncoder[Bar].encode(bar)
      assert(
        string ==
          Right("""
            |<?xml version='1.0' encoding='UTF-8'?>
            | <Bar>
            |   <SomeTopName>d value</SomeTopName>
            |   <SomeFoo SomeName="1" SomeOther="b value">3.0</SomeFoo>
            |   <E>e</E>
            | </Bar>
          """.stripMargin.minimized),
      )
    }

    "encode snake_case" in {
      val snakeCaseConfig = ElementCodecConfig.default.withFieldNamesTransformed(snakeCase)
      case class Foo(@attr someName: Int, @attr someOther: String, @text c: Double)
      case class Bar(someTopName: String, someFoo: Foo, e: Char)

      implicit val fooEncoder: ElementEncoder[Foo] = deriveElementEncoderConfigured(snakeCaseConfig)
      implicit val xmlEncoder: XmlEncoder[Bar]     = deriveXmlEncoderConfigured("bar", snakeCaseConfig)

      val bar    = Bar("d value", Foo(1, "b value", 3.0), 'e')
      val string = XmlEncoder[Bar].encode(bar)
      assert(
        string ==
          Right("""
            |<?xml version='1.0' encoding='UTF-8'?>
            | <bar>
            |   <some_top_name>d value</some_top_name>
            |   <some_foo some_name="1" some_other="b value">3.0</some_foo>
            |   <e>e</e>
            | </bar>
          """.stripMargin.minimized),
      )
    }

    "work for higher kinded data" in {
      case class Foo[F[_]](a: F[Int], b: F[String], c: F[Int])
      implicit val fooOptionEncoder: XmlEncoder[Foo[Option]] = deriveXmlEncoder("foo")
      implicit val fooListEncoder: XmlEncoder[Foo[List]]     = deriveXmlEncoder("foo")

      val fooOption = Foo[Option](Some(123), Some("b value"), None)
      val fooList   = Foo[List](List(123), List("b value 1", "b value 2", "b value 3"), Nil)

      val fooOptionString =
        """<?xml version='1.0' encoding='UTF-8'?>
          | <foo>
          |   <a>123</a>
          |   <b>b value</b>
          | </foo>
        """.stripMargin.minimized
      val fooListString =
        """<?xml version='1.0' encoding='UTF-8'?>
          | <foo>
          |   <a>123</a>
          |   <b>b value 1</b>
          |   <b>b value 2</b>
          |   <b>b value 3</b>
          | </foo>
        """.stripMargin.minimized

      assert(fooOptionEncoder.encode(fooOption) == Right(fooOptionString))
      assert(fooListEncoder.encode(fooList) == Right(fooListString))
    }

    "work for nested higher-kinded data" in {
      case class Foo[F[_]](a: F[Int], b: F[String], c: F[Int])
      implicit val fooOptionEncoder: ElementEncoder[Foo[Option]] = deriveElementEncoder
      implicit val fooListEncoder: ElementEncoder[Foo[List]]     = deriveElementEncoder

      case class Bar[F[_], G[_]](@attr qux: G[String], foo: F[Foo[F]])
      implicit val barOptionEncoder: XmlEncoder[Bar[Option, Option]] = deriveXmlEncoder("bar")
      implicit val barListEncoder: XmlEncoder[Bar[List, Option]]     = deriveXmlEncoder("bar")

      val barOption =
        Bar[Option, Option](Some("qux value"), Some(Foo[Option](Some(123), Some("b value"), None)))
      val barList =
        Bar[List, Option](
          Some("qux value"),
          List(Foo[List](List(123), List("b value 1", "b value 2", "b value 3"), Nil)),
        )

      val barOptionString =
        """<?xml version='1.0' encoding='UTF-8'?>
          | <bar qux="qux value">
          |   <foo>
          |     <a>123</a>
          |     <b>b value</b>
          |   </foo>
          | </bar>
        """.stripMargin.minimized

      val barListString =
        """<?xml version='1.0' encoding='UTF-8'?>
          | <bar qux="qux value">
          |   <foo>
          |     <a>123</a>
          |     <b>b value 1</b>
          |     <b>b value 2</b>
          |     <b>b value 3</b>
          |   </foo>
          | </bar>
        """.stripMargin.minimized

      assert(barOptionEncoder.encode(barOption) == Right(barOptionString))
      assert(barListEncoder.encode(barList) == Right(barListString))
    }
  }

  "Encoder derivation for sealed traits" should {
    "encode simple sealed traits" in {
      case class Bar(d: String, foo: SealedClasses.Foo, e: Char)

      implicit val elementEncoder: ElementEncoder[Bar] = deriveElementEncoder

      val bar1 = Bar("d value", SealedClasses.Foo1("string"), 'k')
      val bar2 = Bar("d value", SealedClasses.Foo2(1), 'e')
      val bar3 = Bar("another one value", SealedClasses.Foo3(1.1234), 'v')

      val barEncoder = XmlEncoder.fromElementEncoder[Bar]("bar")

      val string1 = barEncoder.encode(bar1)
      val string2 = barEncoder.encode(bar2)
      val string3 = barEncoder.encode(bar3)

      assert(
        string1 ==
          Right("""
            | <?xml version='1.0' encoding='UTF-8'?>
            | <bar>
            |   <d>d value</d>
            |   <foo xmlns:ans1="http://www.w3.org/2001/XMLSchema-instance" ans1:type="Foo1">
            |     <a>string</a>
            |   </foo>
            |   <e>k</e>
            | </bar>
          """.stripMargin.minimized) &&
          string2 ==
          Right("""
              | <?xml version='1.0' encoding='UTF-8'?>
              | <bar>
              |   <d>d value</d>
              |   <foo xmlns:ans1="http://www.w3.org/2001/XMLSchema-instance" ans1:type="Foo2">
              |     <b>1</b>
              |   </foo>
              |   <e>e</e>
              | </bar>
          """.stripMargin.minimized) &&
          string3 ==
          Right("""
              | <?xml version='1.0' encoding='UTF-8'?>
              | <bar>
              |   <d>another one value</d>
              |   <foo xmlns:ans1="http://www.w3.org/2001/XMLSchema-instance" ans1:type="Foo3">
              |     <c>1.1234</c>
              |   </foo>
              |   <e>v</e>
              | </bar>
          """.stripMargin.minimized),
      )
    }

    "encode sealed traits with custom discriminator" in {
      case class Qux(d: String, bar: SealedClasses.Bar, e: Char)

      implicit val quxElementEncoder: ElementEncoder[Qux] = deriveElementEncoder

      val qux1 = Qux("d value", SealedClasses.Bar1("string"), 'k')
      val qux2 = Qux("d value", SealedClasses.Bar2(1), 'e')
      val qux3 = Qux("another one value", SealedClasses.Bar3(1.1234), 'v')

      val quxEncoder = XmlEncoder.fromElementEncoder[Qux]("qux")

      val string1 = quxEncoder.encode(qux1)
      val string2 = quxEncoder.encode(qux2)
      val string3 = quxEncoder.encode(qux3)

      assert(
        string1 ==
          Right("""
            | <?xml version='1.0' encoding='UTF-8'?>
            | <qux>
            |   <d>d value</d>
            |   <bar discriminator="Bar1">
            |     <a>string</a>
            |   </bar>
            |   <e>k</e>
            | </qux>
          """.stripMargin.minimized) &&
          string2 ==
          Right("""
              | <?xml version='1.0' encoding='UTF-8'?>
              | <qux>
              |   <d>d value</d>
              |   <bar discriminator="Bar2">
              |     <b>1</b>
              |   </bar>
              |   <e>e</e>
              | </qux>
          """.stripMargin.minimized) &&
          string3 ==
          Right("""
              | <?xml version='1.0' encoding='UTF-8'?>
              | <qux>
              |   <d>another one value</d>
              |   <bar discriminator="Bar3">
              |     <c>1.1234</c>
              |   </bar>
              |   <e>v</e>
              | </qux>
          """.stripMargin.minimized),
      )

      case class Quux(d: String, baz: SealedClasses.Baz, e: Char)

      implicit val quuxElementEncoder: ElementEncoder[Quux] = deriveElementEncoder

      val quux1 = Quux("d value", SealedClasses.Baz1("string"), 'k')
      val quux2 = Quux("d value", SealedClasses.Baz2(1), 'e')
      val quux3 = Quux("another one value", SealedClasses.Baz3(1.1234), 'v')

      val quuxEncoder = XmlEncoder.fromElementEncoder[Quux]("quux")

      val string4 = quuxEncoder.encode(quux1)
      val string5 = quuxEncoder.encode(quux2)
      val string6 = quuxEncoder.encode(quux3)

      assert(
        string4 ==
          Right("""
            | <?xml version='1.0' encoding='UTF-8'?>
            | <quux>
            |   <d>d value</d>
            |   <baz xmlns:ans1="https://tinkoff.ru" ans1:discriminator="Baz1">
            |     <a>string</a>
            |   </baz>
            |   <e>k</e>
            | </quux>
          """.stripMargin.minimized) &&
          string5 ==
          Right("""
              | <?xml version='1.0' encoding='UTF-8'?>
              | <quux>
              |   <d>d value</d>
              |   <baz xmlns:ans1="https://tinkoff.ru" ans1:discriminator="Baz2">
              |     <b>1</b>
              |   </baz>
              |   <e>e</e>
              | </quux>
          """.stripMargin.minimized) &&
          string6 ==
          Right("""
              | <?xml version='1.0' encoding='UTF-8'?>
              | <quux>
              |   <d>another one value</d>
              |   <baz xmlns:ans1="https://tinkoff.ru" ans1:discriminator="Baz3">
              |     <c>1.1234</c>
              |   </baz>
              |   <e>v</e>
              | </quux>
          """.stripMargin.minimized),
      )
    }

    "encode sealed traits with constructor names transformed" in {
      val wolf = SealedClasses.CanisLupus("Igor", 0.2, 20)
      val lion = SealedClasses.PantheraLeo("Sergey", 0.75, 60.1)

      val animalEncoder = XmlEncoder.fromElementEncoder[SealedClasses.Mammalia]("animal")

      assert(
        animalEncoder.encode(wolf) ==
          Right("""
            | <?xml version='1.0' encoding='UTF-8'?>
            | <animal xmlns:ans1="http://www.w3.org/2001/XMLSchema-instance" ans1:type="canis_lupus">
            |   <name>Igor</name>
            |   <strength>0.2</strength>
            |   <age>20</age>
            | </animal>
          """.stripMargin.minimized) &&
          animalEncoder.encode(lion) ==
          Right("""
              | <?xml version='1.0' encoding='UTF-8'?>
              | <animal xmlns:ans1="http://www.w3.org/2001/XMLSchema-instance" ans1:type="panthera_leo">
              |   <name>Sergey</name>
              |   <strength>0.75</strength>
              |   <speed>60.1</speed>
              | </animal>
            """.stripMargin.minimized),
      )
    }

    "encode sealed traits with custom discriminator values" in {
      val hornet    = SealedClasses.Vespa("Anton", 200.123)
      val cockroach = SealedClasses.Blattodea("Dmitriy", 5)

      val insectEncoder = XmlEncoder.fromElementEncoder[SealedClasses.Insecta]("insect")

      assert(
        insectEncoder.encode(hornet) ==
          Right("""
            | <?xml version='1.0' encoding='UTF-8'?>
            | <insect xmlns:ans1="http://www.w3.org/2001/XMLSchema-instance" ans1:type="hornet">
            |   <name>Anton</name>
            |   <damage>200.123</damage>
            | </insect>
          """.stripMargin.minimized) &&
          insectEncoder.encode(cockroach) ==
          Right("""
              | <?xml version='1.0' encoding='UTF-8'?>
              | <insect xmlns:ans1="http://www.w3.org/2001/XMLSchema-instance" ans1:type="cockroach">
              |   <name>Dmitriy</name>
              |   <legsNumber>5</legsNumber>
              | </insect>
            """.stripMargin.minimized),
      )
    }

    "not transform custom discriminator values" in {
      val clownFish  = SealedClasses.Amphiprion("Nemo", 1)
      val whiteShark = SealedClasses.CarcharodonCarcharias("Bill", 20000000000L)

      val fishEncoder = XmlEncoder.fromElementEncoder[SealedClasses.Pisces]("fish")

      assert(
        fishEncoder.encode(clownFish) ==
          Right("""
            | <?xml version='1.0' encoding='UTF-8'?>
            | <fish xmlns:ans1="http://www.w3.org/2001/XMLSchema-instance" ans1:type="ClownFish">
            |   <name>Nemo</name>
            |   <finNumber>1</finNumber>
            | </fish>
          """.stripMargin.minimized) &&
          fishEncoder.encode(whiteShark) ==
          Right("""
              | <?xml version='1.0' encoding='UTF-8'?>
              | <fish xmlns:ans1="http://www.w3.org/2001/XMLSchema-instance" ans1:type="carcharodon_carcharias">
              |   <name>Bill</name>
              |   <teethNumber>20000000000</teethNumber>
              | </fish>
            """.stripMargin.minimized),
      )
    }

    "use element name as discriminator if configured" in {
      case class Zoo(@default animals: List[Animal])

      implicit val xmlEncoder: XmlEncoder[Zoo] = deriveXmlEncoder("zoo")

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
      XmlEncoder[Zoo].encode(zoo) shouldBe Right(string)
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
      animalXmlEncoder.encode(cat) shouldBe Right(catString)
      animalXmlEncoder.encode(dog) shouldBe Right(dogString)
    }
  }

  "Encoder derivation with namespaces" should {
    "encode simple case classes" in {
      case object tkf
      implicit val tkfNs: Namespace[tkf.type] = Namespace.mkInstance("tinkoff.ru")
      case class Foo(
          @xmlns(tkf) a: Int,
          @xmlns(tkf) b: String,
          @xmlns(tkf) c: Double,
      )
      case class Bar(
          @xmlns(tkf) d: String,
          @xmlns(tkf) foo: Foo,
          @xmlns(tkf) e: Char,
      )

      implicit val fooEncoder: ElementEncoder[Foo] = deriveElementEncoder
      implicit val xmlEncoder: XmlEncoder[Bar]     = deriveXmlEncoder("bar", tkf)

      val bar    = Bar("d value", Foo(1, "b value", 3.0), 'e')
      val string = XmlEncoder[Bar].encode(bar)
      assert(
        string ==
          Right("""
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
          """.stripMargin.minimized),
      )
    }

    "encode attributes" in {
      case object tkf
      implicit val tkfNs: Namespace[tkf.type] = Namespace.mkInstance("tinkoff.ru")
      case class Foo(
          @xmlns(tkf) a: Int,
          @xmlns(tkf) @attr b: String,
          @xmlns(tkf) c: Double,
      )
      case class Bar(
          @xmlns(tkf) d: String,
          @xmlns(tkf) foo: Foo,
          @xmlns(tkf) @attr e: Char,
      )

      implicit val fooEncoder: ElementEncoder[Foo] = deriveElementEncoder
      implicit val xmlEncoder: XmlEncoder[Bar]     = deriveXmlEncoder("bar", tkf)

      val bar    = Bar("d value", Foo(1, "b value", 3.0), 'e')
      val string = XmlEncoder[Bar].encode(bar)
      assert(
        string ==
          Right("""
            | <?xml version='1.0' encoding='UTF-8'?>
            | <ans1:bar xmlns:ans1="tinkoff.ru" ans1:e="e">
            |   <ans1:d>d value</ans1:d>
            |   <ans1:foo ans1:b="b value">
            |     <ans1:a>1</ans1:a>
            |     <ans1:c>3.0</ans1:c>
            |   </ans1:foo>
            | </ans1:bar>
          """.stripMargin.minimized),
      )
    }

    "encode nested namespace" in {
      case object tkf
      implicit val tkfNs: Namespace[tkf.type] = Namespace.mkInstance("tinkoff.ru")
      case class Foo(
          @xmlns(tkf) a: Int,
          @attr b: String,
          @xmlns(tkf) c: Double,
      )
      case class Bar(
          d: String,
          @xmlns(tkf) foo: Foo,
          @attr e: Char,
      )

      implicit val fooEncoder: ElementEncoder[Foo] = deriveElementEncoder
      implicit val xmlEncoder: XmlEncoder[Bar]     = deriveXmlEncoder("bar")

      val bar    = Bar("d value", Foo(1, "b value", 3.0), 'e')
      val string = XmlEncoder[Bar].encode(bar)
      assert(
        string ==
          Right("""
            | <?xml version='1.0' encoding='UTF-8'?>
            | <bar e="e">
            |   <d>d value</d>
            |   <ans1:foo xmlns:ans1="tinkoff.ru" b="b value">
            |     <ans1:a>1</ans1:a>
            |     <ans1:c>3.0</ans1:c>
            |   </ans1:foo>
            | </bar>
          """.stripMargin.minimized),
      )
    }

    "encode multiple namespaces" in {
      case object tkf
      implicit val tkfNs: Namespace[tkf.type] = Namespace.mkInstance("tinkoff.ru")
      case object tcs
      implicit val tcsNs: Namespace[tcs.type] = Namespace.mkInstance("tcsbank.ru")
      case class Foo(
          @xmlns(tkf) a: Int,
          @attr b: String,
          @xmlns(tkf) c: Double,
      )
      case class Bar(
          @xmlns(tcs) d: String,
          @xmlns(tkf) foo: Foo,
          @attr e: Char,
      )

      implicit val fooEncoder: ElementEncoder[Foo] = deriveElementEncoder
      implicit val xmlEncoder: XmlEncoder[Bar]     = deriveXmlEncoder("bar", tcs)

      val bar    = Bar("d value", Foo(1, "b value", 3.0), 'e')
      val string = XmlEncoder[Bar].encode(bar)
      assert(
        string ==
          Right("""
            | <?xml version='1.0' encoding='UTF-8'?>
            | <ans1:bar xmlns:ans1="tcsbank.ru" e="e">
            |   <ans1:d>d value</ans1:d>
            |   <ans2:foo xmlns:ans2="tinkoff.ru" b="b value">
            |     <ans2:a>1</ans2:a>
            |     <ans2:c>3.0</ans2:c>
            |   </ans2:foo>
            | </ans1:bar>
          """.stripMargin.minimized),
      )
    }

    "encode CamelCase" in {
      val camelCaseConfig = ElementCodecConfig.default.withFieldNamesTransformed(camelCase)
      case object tkf
      implicit val tkfNs: Namespace[tkf.type] = Namespace.mkInstance("tinkoff.ru")
      case class Foo(
          @xmlns(tkf) someName: Int,
          @xmlns(tkf) someOtherName: String,
          @xmlns(tkf) c: Double,
      )
      case class Bar(
          @xmlns(tkf) someTopName: String,
          @xmlns(tkf) someFoo: Foo,
      )

      implicit val fooEncoder: ElementEncoder[Foo] = deriveElementEncoderConfigured(camelCaseConfig)
      implicit val xmlEncoder: XmlEncoder[Bar]     = deriveXmlEncoderConfigured("Bar", tkf, camelCaseConfig)

      val bar    = Bar("d value", Foo(1, "b value", 3.0))
      val string = XmlEncoder[Bar].encode(bar)
      assert(
        string ==
          Right("""
            | <?xml version='1.0' encoding='UTF-8'?>
            | <ans1:Bar xmlns:ans1="tinkoff.ru">
            |   <ans1:SomeTopName>d value</ans1:SomeTopName>
            |   <ans1:SomeFoo>
            |     <ans1:SomeName>1</ans1:SomeName>
            |     <ans1:SomeOtherName>b value</ans1:SomeOtherName>
            |     <ans1:C>3.0</ans1:C>
            |   </ans1:SomeFoo>
            | </ans1:Bar>
          """.stripMargin.minimized),
      )
    }

    "encode snake_case" in {
      val snakeCaseConfig = ElementCodecConfig.default.withFieldNamesTransformed(snakeCase)
      case object tkf
      implicit val tkfNs: Namespace[tkf.type] = Namespace.mkInstance("tinkoff.ru")
      case class Foo(
          @xmlns(tkf) someName: Int,
          @xmlns(tkf) someOtherName: String,
          @xmlns(tkf) c: Double,
      )
      case class Bar(
          @xmlns(tkf) someTopName: String,
          @xmlns(tkf) someFoo: Foo,
      )

      implicit val fooEncoder: ElementEncoder[Foo] = deriveElementEncoderConfigured(snakeCaseConfig)
      implicit val xmlEncoder: XmlEncoder[Bar]     = deriveXmlEncoderConfigured("bar", tkf, snakeCaseConfig)

      val bar    = Bar("d value", Foo(1, "b value", 3.0))
      val string = XmlEncoder[Bar].encode(bar)
      assert(
        string ==
          Right("""
            | <?xml version='1.0' encoding='UTF-8'?>
            | <ans1:bar xmlns:ans1="tinkoff.ru">
            |   <ans1:some_top_name>d value</ans1:some_top_name>
            |   <ans1:some_foo>
            |     <ans1:some_name>1</ans1:some_name>
            |     <ans1:some_other_name>b value</ans1:some_other_name>
            |     <ans1:c>3.0</ans1:c>
            |   </ans1:some_foo>
            | </ans1:bar>
          """.stripMargin.minimized),
      )
    }

    "encode with @renamed having priority over naming" in {
      val snakeCaseConfig = ElementCodecConfig.default.withFieldNamesTransformed(snakeCase)
      case class Foo(@attr someName: Int, @attr @renamed("i-Have-priority") someOther: String, @text c: Double)
      case class Bar(someTopName: String, @renamed("Me2") someFoo: Foo, e: Char)

      implicit val fooEncoder: ElementEncoder[Foo] = deriveElementEncoderConfigured(snakeCaseConfig)
      implicit val xmlEncoder: XmlEncoder[Bar]     = deriveXmlEncoderConfigured("bar", snakeCaseConfig)

      val bar = Bar("d value", Foo(1, "b value", 3.0), 'e')

      val string = XmlEncoder[Bar].encode(bar)
      assert(
        string ==
          Right("""<?xml version='1.0' encoding='UTF-8'?>
            | <bar>
            |   <some_top_name>d value</some_top_name>
            |   <Me2 some_name="1" i-Have-priority="b value">3.0</Me2>
            |   <e>e</e>
            | </bar>
          """.stripMargin.minimized),
      )
    }

    "encode with default element namespaces" in {
      case object tkf
      implicit val tkfNs: Namespace[tkf.type] = Namespace.mkInstance("tinkoff.ru")

      val defaultNamespaceConfig = ElementCodecConfig.default.withElementsDefaultNamespace(tkf)

      case class Foo(a: Int, b: String, c: Double)
      case class Bar(@attr d: String, foo: Foo, e: Char)

      implicit val fooEncoder: ElementEncoder[Foo] = deriveElementEncoder
      implicit val xmlEncoder: XmlEncoder[Bar]     = deriveXmlEncoderConfigured("bar", tkf, defaultNamespaceConfig)

      val bar    = Bar("d value", Foo(1, "b value", 3.0), 'e')
      val string = XmlEncoder[Bar].encode(bar)

      assert(
        string ==
          Right("""<?xml version='1.0' encoding='UTF-8'?>
          | <ans1:bar xmlns:ans1="tinkoff.ru" d="d value">
          |   <ans1:foo>
          |     <a>1</a>
          |     <b>b value</b>
          |     <c>3.0</c>
          |   </ans1:foo>
          |  <ans1:e>e</ans1:e>
          | </ans1:bar>
      """.stripMargin.minimized),
      )
    }

    "override default element namespace with namespace from annotation" in {
      case object tkf
      implicit val tkfNs: Namespace[tkf.type] = Namespace.mkInstance("tinkoff.ru")

      case object tcs
      implicit val tcsNs: Namespace[tcs.type] = Namespace.mkInstance("tcsbank.ru")

      val defaultNamespaceConfig = ElementCodecConfig.default.withElementsDefaultNamespace(tkf)
      case class Foo(a: Int, b: String, c: Double)
      case class Bar(@attr d: String, @xmlns(tcs) foo: Foo, e: Char)

      implicit val fooEncoder: ElementEncoder[Foo] = deriveElementEncoder
      implicit val xmlEncoder: XmlEncoder[Bar]     = deriveXmlEncoderConfigured("bar", tkf, defaultNamespaceConfig)

      val bar    = Bar("d value", Foo(1, "b value", 3.0), 'e')
      val string = XmlEncoder[Bar].encode(bar)

      assert(
        string ==
          Right("""<?xml version='1.0' encoding='UTF-8'?>
            | <ans1:bar xmlns:ans1="tinkoff.ru" d="d value">
            |   <ans2:foo xmlns:ans2="tcsbank.ru">
            |     <a>1</a>
            |     <b>b value</b>
            |     <c>3.0</c>
            |   </ans2:foo>
            |  <ans1:e>e</ans1:e>
            | </ans1:bar>
      """.stripMargin.minimized),
      )
    }

    "encode with default attribute namespaces" in {
      case object tkf
      implicit val tkfNs: Namespace[tkf.type] = Namespace.mkInstance("tinkoff.ru")

      val defaultNamespaceConfig = ElementCodecConfig.default.withAttributesDefaultNamespace(tkf)
      case class Foo(a: Int, b: String, c: Double)
      case class Bar(@attr d: String, foo: Foo, @attr e: Char)

      implicit val fooEncoder: ElementEncoder[Foo] = deriveElementEncoder
      implicit val xmlEncoder: XmlEncoder[Bar]     = deriveXmlEncoderConfigured("bar", tkf, defaultNamespaceConfig)

      val bar    = Bar("d value", Foo(1, "b value", 3.0), 'e')
      val string = XmlEncoder[Bar].encode(bar)

      assert(
        string ==
          Right("""<?xml version='1.0' encoding='UTF-8'?>
            | <ans1:bar xmlns:ans1="tinkoff.ru" ans1:d="d value" ans1:e="e">
            |   <foo>
            |     <a>1</a>
            |     <b>b value</b>
            |     <c>3.0</c>
            |   </foo>
            | </ans1:bar>
      """.stripMargin.minimized),
      )
    }

    "override default attribute namespace with namespace from annotation" in {
      case object tkf
      implicit val tkfNs: Namespace[tkf.type] = Namespace.mkInstance("tinkoff.ru")

      case object tcs
      implicit val tcsNs: Namespace[tcs.type] = Namespace.mkInstance("tcsbank.ru")

      val defaultNamespaceConfig = ElementCodecConfig.default.withAttributesDefaultNamespace(tkf)
      case class Foo(a: Int, b: String, c: Double)
      case class Bar(@attr d: String, foo: Foo, @attr @xmlns(tcs) e: Char)

      implicit val fooEncoder: ElementEncoder[Foo] = deriveElementEncoder
      implicit val xmlEncoder: XmlEncoder[Bar]     = deriveXmlEncoderConfigured("bar", tkf, defaultNamespaceConfig)

      val bar    = Bar("d value", Foo(1, "b value", 3.0), 'e')
      val string = XmlEncoder[Bar].encode(bar)

      assert(
        string ==
          Right("""<?xml version='1.0' encoding='UTF-8'?>
            | <ans1:bar xmlns:ans1="tinkoff.ru" ans1:d="d value" xmlns:ans2="tcsbank.ru" ans2:e="e">
            |   <foo>
            |     <a>1</a>
            |     <b>b value</b>
            |     <c>3.0</c>
            |   </foo>
            | </ans1:bar>
      """.stripMargin.minimized),
      )
    }

    "define namespaces from config" in {
      case object tkf
      implicit val tkfNs: Namespace[tkf.type] = Namespace.mkInstance("tinkoff.ru")

      case object tcs
      implicit val tcsNs: Namespace[tcs.type] = Namespace.mkInstance("tcsbank.ru")

      val config = ElementCodecConfig.default.withNamespaceDefined(tkf).withNamespaceDefined(tcs)

      final case class Foo(
          @xmlns(tcs) a: Int,
          @xmlns(tcs) b: String,
          @xmlns(tcs) c: Double,
      )

      implicit val xmlEncoder: XmlEncoder[Foo] = deriveXmlEncoderConfigured("foo", config)

      val foo    = Foo(1, "b value", 3.0)
      val string = XmlEncoder[Foo].encode(foo)

      assert(
        string ==
          Right("""<?xml version='1.0' encoding='UTF-8'?>
            | <foo xmlns:ans1="tcsbank.ru" xmlns:ans2="tinkoff.ru">
            |   <ans1:a>1</ans1:a>
            |   <ans1:b>b value</ans1:b>
            |   <ans1:c>3.0</ans1:c>
            | </foo>
      """.stripMargin.minimized),
      )
    }

    "define namespaces with not bounded prefixes" in {
      case object tkf
      implicit val tkfNs: Namespace[tkf.type] = Namespace.mkInstance("tinkoff.ru")

      case object tcs
      implicit val tcsNs: Namespace[tcs.type] = Namespace.mkInstance("tcsbank.ru")

      val config =
        List
          .range(10, 2, -1)
          .foldLeft(ElementCodecConfig.default)((config, i) => config.withNamespaceDefined(s"example.com/$i"))

      final case class Foo(
          a: Int,
          b: String,
          c: Double,
      )

      final case class Bar(
          @xmlns(tcs) foo: Foo,
      )

      implicit val fooEncoder: ElementEncoder[Foo] = deriveElementEncoderConfigured(config)
      implicit val xmlEncoder: XmlEncoder[Bar]     = deriveXmlEncoder("bar", tkf)

      val bar    = Bar(Foo(1, "b value", 3.0))
      val string = XmlEncoder[Bar].encode(bar)
      assert(
        string ==
          Right("""<?xml version='1.0' encoding='UTF-8'?>
          | <ans1:bar xmlns:ans1="tinkoff.ru">
          |   <ans2:foo xmlns:ans2="tcsbank.ru" xmlns:ans3="example.com/3" xmlns:ans4="example.com/4" 
          |             xmlns:ans5="example.com/5" xmlns:ans6="example.com/6" xmlns:ans7="example.com/7" 
          |             xmlns:ans8="example.com/8" xmlns:ans9="example.com/9" xmlns:ans10="example.com/10">
          |     <a>1</a>
          |     <b>b value</b>
          |     <c>3.0</c>
          |   </ans2:foo>
          | </ans1:bar>
      """.stripMargin.minimized),
      )
    }

    "not define already defined namespaces" in {
      case object tkf
      implicit val tkfNs: Namespace[tkf.type] = Namespace.mkInstance("tinkoff.ru")

      case object tcs
      implicit val tcsNs: Namespace[tcs.type] = Namespace.mkInstance("tcsbank.ru")

      val config = ElementCodecConfig.default.withNamespaceDefined(tkf).withNamespaceDefined(tcs)
      final case class Foo(
          a: Int,
          b: String,
          c: Double,
      )

      final case class Bar(
          foo: Foo,
      )

      implicit val fooEncoder: ElementEncoder[Foo] = deriveElementEncoderConfigured(config)
      implicit val xmlEncoder: XmlEncoder[Bar]     = deriveXmlEncoder("bar", tkf)

      val bar    = Bar(Foo(1, "b value", 3.0))
      val string = XmlEncoder[Bar].encode(bar)
      assert(
        string ==
          Right("""<?xml version='1.0' encoding='UTF-8'?>
            | <ans1:bar xmlns:ans1="tinkoff.ru">
            |   <foo xmlns:ans2="tcsbank.ru">
            |     <a>1</a>
            |     <b>b value</b>
            |     <c>3.0</c>
            |   </foo>
            | </ans1:bar>
      """.stripMargin.minimized),
      )
    }

    "encode sealed traits" in {
      case object tkf
      implicit val tkfNs: Namespace[tkf.type] = Namespace.mkInstance("tinkoff.ru")

      case class Aquarium(@xmlns(tkf) fish: List[SealedClasses.Pisces])
      implicit val xmlEncoder: XmlEncoder[Aquarium] =
        deriveXmlEncoderConfigured("aquarium", ElementCodecConfig.default.withNamespaceDefined(tkf))

      val string =
        """<?xml version='1.0' encoding='UTF-8'?>
          | <aquarium xmlns:ans1="tinkoff.ru">
          |   <ans1:fish xmlns:ans2="http://www.w3.org/2001/XMLSchema-instance" ans2:type="ClownFish">
          |     <name>Marlin</name>
          |     <finNumber>3</finNumber>
          |   </ans1:fish>
          |   <ans1:fish xmlns:ans3="http://www.w3.org/2001/XMLSchema-instance" ans3:type="carcharodon_carcharias">
          |     <name>Jaws</name>
          |     <teethNumber>1234</teethNumber>
          |   </ans1:fish>
          | </aquarium>
          |""".stripMargin.minimized
      val aquarium =
        Aquarium(List(SealedClasses.Amphiprion("Marlin", 3), SealedClasses.CarcharodonCarcharias("Jaws", 1234)))
      XmlEncoder[Aquarium].encode(aquarium) shouldBe Right(string)
    }

    "encode sealed traits using element names as discriminators" in {
      case object tkf
      implicit val tkfNs: Namespace[tkf.type] = Namespace.mkInstance("tinkoff.ru")

      case class AnimalShelter(@xmlns(tkf) animals: List[SealedClasses.Animal])
      implicit val xmlEncoder: XmlEncoder[AnimalShelter] =
        deriveXmlEncoderConfigured("shelter", ElementCodecConfig.default.withNamespaceDefined(tkf))

      val string =
        """<?xml version='1.0' encoding='UTF-8'?>
          | <shelter xmlns:ans1="tinkoff.ru">
          |   <ans1:cat>
          |     <meow>meow</meow>
          |   </ans1:cat>
          |   <ans1:dog>
          |     <woof>1234</woof>
          |   </ans1:dog>
          | </shelter>
        """.stripMargin.minimized
      val animalShelter = AnimalShelter(List(Cat("meow"), Dog(1234)))
      XmlEncoder[AnimalShelter].encode(animalShelter) shouldBe Right(string)
    }

    "encode namespaces with preferred prefixes" in {
      case object tkf
      implicit val tkfNs: Namespace[tkf.type] = Namespace.mkInstance("tinkoff.ru", preferredPrefix = Some("tkf"))

      case object tkf2
      implicit val tkfNs2: Namespace[tkf2.type] = Namespace.mkInstance("tinkoff.ru")

      case class Bar(@xmlns(tkf2) c: String, @xmlns(tkf) d: String)
      implicit val barEncoder: ElementEncoder[Bar] = deriveElementEncoder
      case class Foo(@xmlns(tkf) a: String, @xmlns(tkf) b: Int, @xmlns(tkf) bar: Bar)
      implicit val fooEncoder: XmlEncoder[Foo] = deriveXmlEncoder("foo", tkf)

      val foo = Foo("string", 1, Bar("gnirts", "string"))
      val string =
        """<?xml version='1.0' encoding='UTF-8'?>
          | <tkf:foo xmlns:tkf="tinkoff.ru">
          |   <tkf:a>string</tkf:a>
          |   <tkf:b>1</tkf:b>
          |   <tkf:bar>
          |     <tkf:c>gnirts</tkf:c>
          |     <tkf:d>string</tkf:d>
          |   </tkf:bar>
          | </tkf:foo>
        """.stripMargin.minimized

      XmlEncoder[Foo].encode(foo) shouldBe Right(string)
    }

    "declare namespaces with preferred prefixes from config" in {
      case object tkf
      implicit val tkfNs: Namespace[tkf.type] = Namespace.mkInstance("tinkoff.ru", preferredPrefix = Some("tkf"))

      case object tcs
      implicit val tcsNs: Namespace[tcs.type] = Namespace.mkInstance("tcsbank.ru", preferredPrefix = Some("tcs"))

      val config = ElementCodecConfig.default.withNamespaceDefined(tkf).withNamespaceDefined(tcs)

      final case class Foo(
          @xmlns(tcs) a: Int,
          @xmlns(tcs) b: String,
          @xmlns(tcs) c: Double,
      )

      implicit val xmlEncoder: XmlEncoder[Foo] = deriveXmlEncoderConfigured("foo", config)

      val foo    = Foo(1, "b value", 3.0)
      val string = XmlEncoder[Foo].encode(foo)

      assert(
        string ==
          Right("""<?xml version='1.0' encoding='UTF-8'?>
            | <foo xmlns:tcs="tcsbank.ru" xmlns:tkf="tinkoff.ru">
            |   <tcs:a>1</tcs:a>
            |   <tcs:b>b value</tcs:b>
            |   <tcs:c>3.0</tcs:c>
            | </foo>
     """.stripMargin.minimized),
      )
    }

    "declare scope namespace from config" in {
      case object tkf
      implicit val tkfNs: Namespace[tkf.type] = Namespace.mkInstance("tinkoff.ru")

      case object tcs
      implicit val tcsNs: Namespace[tcs.type] = Namespace.mkInstance("tcsbank.ru")

      final case class Qux(g: String, h: Int)
      implicit val quxEncoder: ElementEncoder[Qux] = deriveElementEncoder

      final case class Foo(
          d: Int,
          @xmlns(tcs) e: String,
          f: Double,
          qux: List[Qux],
      )
      implicit val fooEncoder: ElementEncoder[Foo] = deriveElementEncoder

      final case class Bar(@xmlns(tcs) a: Int, b: String, c: Double, foo: Foo)
      val config                               = ElementCodecConfig.default.withScopeDefaultNamespace(tkf)
      implicit val barEncoder: XmlEncoder[Bar] = deriveXmlEncoderConfigured("bar", config)

      val bar    = Bar(123, "b value", 1.234, Foo(321, "e value", 4.321, List(Qux("g value 1", 1), Qux("g value 2", 2))))
      val string = XmlEncoder[Bar].encode(bar)
      assert(
        string ==
          Right("""<?xml version='1.0' encoding='UTF-8'?>
          | <bar xmlns="tinkoff.ru">
          |   <ans1:a xmlns:ans1="tcsbank.ru">123</ans1:a>
          |   <b>b value</b>
          |   <c>1.234</c>
          |   <foo>
          |     <d>321</d>
          |     <ans2:e xmlns:ans2="tcsbank.ru">e value</ans2:e>
          |     <f>4.321</f>
          |     <qux>
          |       <g>g value 1</g>
          |       <h>1</h>
          |     </qux>
          |     <qux>
          |       <g>g value 2</g>
          |       <h>2</h>
          |     </qux>
          |   </foo>
          | </bar>
          |""".stripMargin.minimized),
      )
    }

    "declare nested scope namespaces from config" in {
      case object tkf
      implicit val tkfNs: Namespace[tkf.type] = Namespace.mkInstance("tinkoff.ru")

      case object tcs
      implicit val tcsNs: Namespace[tcs.type] = Namespace.mkInstance("tcsbank.ru")

      case object xmp
      implicit val xmlNs: Namespace[xmp.type] = Namespace.mkInstance("example.org")

      final case class Foo(
          d: Int,
          @xmlns(tcs) e: String,
          f: Double,
      )
      val fooConfig                                = ElementCodecConfig.default.withScopeDefaultNamespace(xmp)
      implicit val fooEncoder: ElementEncoder[Foo] = deriveElementEncoderConfigured(fooConfig)

      final case class Bar(@xmlns(tcs) a: Int, b: String, c: Double, foo: Foo)
      val barConfig                            = ElementCodecConfig.default.withScopeDefaultNamespace(tkf)
      implicit val barEncoder: XmlEncoder[Bar] = deriveXmlEncoderConfigured("bar", barConfig)

      val bar    = Bar(123, "b value", 1.234, Foo(321, "e value", 4.321))
      val string = XmlEncoder[Bar].encode(bar)
      assert(
        string ==
          Right("""<?xml version='1.0' encoding='UTF-8'?>
            | <bar xmlns="tinkoff.ru">
            |   <ans1:a xmlns:ans1="tcsbank.ru">123</ans1:a>
            |   <b>b value</b>
            |   <c>1.234</c>
            |   <foo xmlns="example.org">
            |     <d>321</d>
            |     <ans2:e xmlns:ans2="tcsbank.ru">e value</ans2:e>
            |     <f>4.321</f>
            |   </foo>
            | </bar>
            |""".stripMargin.minimized),
      )
    }
  }

  "Encoder derivation compilation" should {
    "fail if wrong attributes" in {
      """
        | case class NotAttribute(a: Int)
        | implicit val notAttributeEncoder: ElementEncoder[NotAttribute] = deriveElementEncoder
        | case class Wrapper(@attr attribute: NotAttribute)
        | implicit val wrapperEncoder: ElementEncoder[Wrapper] = deriveElementEncoder
      """.stripMargin shouldNot typeCheck
    }

    "fail if wrong text" in {
      """
        | case class NotText(a: Int)
        | implicit val notTextEncoder: ElementEncoder[NotText] = deriveElementEncoder
        | case class Wrapper(@text text: NotText, @attr a: Int)
        | implicit val wrapperEncoder: ElementEncoder[Wrapper] = deriveElementEncoder
      """.stripMargin shouldNot typeCheck
    }

    "fail if multiple xml annotations" in {
      """
        | case class AttrText(@attr @text attrText: Int, b: String)
        | implicit val attrTextEncoder: ElementEncoder[AttrText] = deriveElementEncoder
      """.stripMargin shouldNot typeCheck
    }

    "fail if multiple texts" in {
      """
        | case class MultiText(@text a: Int, @text b: String)
        | implicit val multiTextEncoder: ElementEncoder[MultiText] = deriveElementEncoder
      """.stripMargin shouldNot typeCheck
    }
  }
}
