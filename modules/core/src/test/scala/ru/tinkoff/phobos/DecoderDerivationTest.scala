package ru.tinkoff.phobos

import org.scalatest.Assertion
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import ru.tinkoff.phobos.SealedClasses.{Animal, Cat, Cow, Dog}
import ru.tinkoff.phobos.decoding.{AttributeDecoder, DecodingError, ElementDecoder, TextDecoder, XmlDecoder}
import ru.tinkoff.phobos.syntax._
import ru.tinkoff.phobos.configured.naming._
import ru.tinkoff.phobos.configured.ElementCodecConfig
import ru.tinkoff.phobos.derivation.semiauto._

import scala.annotation.nowarn

@nowarn("msg=is never used")
class DecoderDerivationTest extends AnyWordSpec with Matchers {
  def pure(str: String): List[Array[Byte]] =
    List(str.getBytes("UTF-8"))

  def fromIterable(str: String): List[Array[Byte]] =
    str.toList.map(c => Array(c.toByte))

  "Decoder derivation without namespaces" should {

    def decodeSimpleCaseClasses(toList: String => List[Array[Byte]]): Assertion = {
      case class Foo(a: Int, b: String, c: Double)
      case class Bar(d: String, foo: Foo, e: Char)

      implicit val fooDecoder: ElementDecoder[Foo] = deriveElementDecoder
      implicit val xmlDecoder: XmlDecoder[Bar]     = deriveXmlDecoder("bar")

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

      val decoded = XmlDecoder[Bar].decodeFromIterable(toList(string))
      assert(decoded == Right(bar))
    }

    "decode simple case classes sync" in decodeSimpleCaseClasses(pure)
    "decode simple case classes async" in decodeSimpleCaseClasses(fromIterable)

    def decodeAttributes(toList: String => List[Array[Byte]]): Assertion = {
      case class Foo(a: Int, @attr b: String, c: Double)
      case class Bar(d: String, foo: Foo, @attr e: Char)

      implicit val fooDecoder: ElementDecoder[Foo] = deriveElementDecoder
      implicit val xmlDecoder: XmlDecoder[Bar]     = deriveXmlDecoder("bar")

      val bar = Bar("d value", Foo(1, "b value", 3.0), 'e')

      val string = """<?xml version='1.0' encoding='UTF-8'?>
                     | <bar e="e">
                     |   <d>d value</d>
                     |   <foo b="b value">
                     |     <a>1</a>
                     |     <c>3.0</c>
                     |   </foo>
                     | </bar>
                   """.stripMargin
      val decoded = XmlDecoder[Bar].decodeFromIterable(toList(string))
      assert(decoded == Right(bar))
    }

    "decode attributes sync" in decodeAttributes(pure)
    "decode attributes async" in decodeAttributes(fromIterable)

    def allowToOverrideCodecs(toList: String => List[Array[Byte]]): Assertion = {
      implicit val alternativeElementDecoder: ElementDecoder[String] =
        ElementDecoder.stringDecoder.map(_ => "constant")
      implicit val alternativeAttributeDecoder: AttributeDecoder[Int] =
        AttributeDecoder.stringDecoder.map(_ => 24)
      implicit val alternativeTextDecoder: TextDecoder[Double] =
        TextDecoder.stringDecoder.map(_ => -42.0)

      case class Foo(@attr bar: Int, @text baz: Double)
      object Foo {
        implicit val fooDecoder: ElementDecoder[Foo] = deriveElementDecoder
      }
      case class Qux(str: String, foo: Foo)
      object Qux {
        implicit val quxDecoder: XmlDecoder[Qux] = deriveXmlDecoder("qux")
      }

      val qux = Qux("constant", Foo(24, -42.0))
      val string =
        """<?xml version='1.0' encoding='UTF-8'?>
          | <qux>
          |   <str>Not that constant</str>
          |   <foo bar="not number">not number</foo>
          | </qux>
          """.stripMargin
      val decoded = XmlDecoder[Qux].decodeFromIterable(toList(string))
      assert(decoded == Right(qux))

    }

    "allow to override codecs sync" in allowToOverrideCodecs(pure)
    "allow to override codecs async" in allowToOverrideCodecs(fromIterable)

    def decodeOptions(toList: String => List[Array[Byte]]): Assertion = {
      case class Foo(a: Option[Int], @attr b: String, c: Option[Double])
      case class Wrapper(foo: Option[Foo])

      implicit val fooDecoder: ElementDecoder[Foo] = deriveElementDecoder
      implicit val xmlDecoder: XmlDecoder[Wrapper] = deriveXmlDecoder("Wrapper")

      val opt1 = Wrapper(Some(Foo(Some(1), "b", Some(2.0))))
      val opt2 = Wrapper(None)
      val opt3 = Wrapper(Some(Foo(None, "b", None)))

      val string1 = """<?xml version='1.0' encoding='UTF-8'?>
                      | <Wrapper>
                      |   <foo b="b">
                      |     <a>1</a>
                      |     <c>2.0</c>
                      |   </foo>
                      | </Wrapper>
                    """.stripMargin
      val string2 = """<?xml version='1.0' encoding='UTF-8'?>
                      | <Wrapper/>
                    """.stripMargin
      val string3 = """<?xml version='1.0' encoding='UTF-8'?>
                       | <Wrapper>
                       |   <foo b="b"/>
                       | </Wrapper>
                    """.stripMargin
      val decoded1 = XmlDecoder[Wrapper].decodeFromIterable(toList(string1))
      val decoded2 = XmlDecoder[Wrapper].decodeFromIterable(toList(string2))
      val decoded3 = XmlDecoder[Wrapper].decodeFromIterable(toList(string3))
      assert(decoded1 == Right(opt1) && decoded2 == Right(opt2) && decoded3 == Right(opt3))
    }

    "decode options sync" in decodeOptions(pure)
    "decode options async" in decodeOptions(fromIterable)

    "decode malformed XML results in Left informing where the error occurred" in {
      case class Foo(a: Int, @attr b: String)
      case class Wrapper(foo: List[Foo])

      implicit val fooDecoder: ElementDecoder[Foo] = deriveElementDecoder
      implicit val xmlDecoder: XmlDecoder[Wrapper] = deriveXmlDecoder("Wrapper")

      val invalidXmlStringAtFoo =
        """<?xml version='1.0' encoding='UTF-8'?>
          | <Wrapper>
          |   <foo>iufeiuf<foo>
          | </Wrapper>""".stripMargin

      val totallyInvalidXml = "LOL"

      val decodedResultInvalidFoo   = XmlDecoder[Wrapper].decode(invalidXmlStringAtFoo)
      val decodedResultInvalidTotal = XmlDecoder[Wrapper].decode(totallyInvalidXml)

      assert(
        decodedResultInvalidFoo == Left(
          DecodingError(
            "Unexpected end tag: expected </foo>\n at [row,col {unknown-source}]: [3,8]",
            List("foo", "foo", "Wrapper"),
          ),
        ),
      )

      assert(
        decodedResultInvalidTotal == Left(
          DecodingError("Unexpected character 'L' (code 76) in prolog\n at [row,col {unknown-source}]: [1,2]", Nil),
        ),
      )
    }

    def decodeNilValues(toList: String => List[Array[Byte]]): Assertion = {
      case class Foo(a: Int, @attr b: String)
      case class Wrapper1(foo: List[Foo])
      case class Wrapper2(foo: Option[Foo])

      implicit val fooDecoder: ElementDecoder[Foo]   = deriveElementDecoder
      implicit val xmlDecoder1: XmlDecoder[Wrapper1] = deriveXmlDecoder("Wrapper1")
      implicit val xmlDecoder2: XmlDecoder[Wrapper2] = deriveXmlDecoder("Wrapper2")

      val wrapper1 = Wrapper1(List(Foo(1, "b"), Foo(2, "b2")))
      val wrapper2 = Wrapper2(None)

      val string1 = """<?xml version='1.0' encoding='UTF-8'?>
                      | <Wrapper1 xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
                      |   <foo xsi:nil="true"></foo>
                      |   <foo b="b">
                      |     <a>1</a>
                      |   </foo>
                      |   <foo xsi:nil="true"></foo>
                      |   <foo b="b2">
                      |     <a>2</a>
                      |   </foo>
                      | </Wrapper1>
                    """.stripMargin

      val string2 = """<?xml version='1.0' encoding='UTF-8'?>
                      | <Wrapper2 xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
                      |    <foo xsi:nil="true"></foo>
                      | </Wrapper2>
                    """.stripMargin

      val decoded1 = XmlDecoder[Wrapper1].decodeFromIterable(toList(string1))
      val decoded2 = XmlDecoder[Wrapper2].decodeFromIterable(toList(string2))
      assert(decoded1 == Right(wrapper1) && decoded2 == Right(wrapper2))
    }

    "decode nil values sync" in decodeNilValues(pure)
    "decode nil values async" in decodeNilValues(fromIterable)

    def decodeLists(toList: String => List[Array[Byte]]): Assertion = {
      case class Foo(a: Int, @attr b: Option[String], c: Option[Double])
      case class Foos(foo: List[Foo])

      implicit val fooDecoder: ElementDecoder[Foo] = deriveElementDecoder
      implicit val xmlDecoder: XmlDecoder[Foos]    = deriveXmlDecoder("foos")

      val bar1 = Foos(
        List(
          Foo(1, Some("b value"), Some(3.0)),
          Foo(2, None, Some(4.0)),
          Foo(3, Some("It's three"), None),
          Foo(4, None, None),
        ),
      )
      val bar2 = Foos(List())

      val string1 = """<?xml version='1.0' encoding='UTF-8'?>
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
                    """.stripMargin

      val string2 =
        """<?xml version='1.0' encoding='UTF-8'?>
          | <foos/>
        """.stripMargin

      val decoded1 = XmlDecoder[Foos].decodeFromIterable(toList(string1))
      val decoded2 = XmlDecoder[Foos].decodeFromIterable(toList(string2))
      assert(decoded1 == Right(bar1) && decoded2 == Right(bar2))
    }

    "decode lists sync" in decodeLists(pure)
    "decode lists async" in decodeLists(fromIterable)

    def decodeMixedLists(toList: String => List[Array[Byte]]): Assertion = {
      case class Foo(a: Int, @attr b: Option[String], c: Option[Double])
      case class Bar(a: String, b: Int)
      case class FooBars(foo: List[Foo], bar: List[Bar])

      implicit val fooDecoder: ElementDecoder[Foo] = deriveElementDecoder
      implicit val barDecoder: ElementDecoder[Bar] = deriveElementDecoder
      implicit val xmlDecoder: XmlDecoder[FooBars] = deriveXmlDecoder("foobars")

      val bar1 = FooBars(
        List(
          Foo(1, Some("b value"), Some(3.0)),
          Foo(2, None, Some(4.0)),
          Foo(3, Some("It's three"), None),
          Foo(4, None, None),
        ),
        List(Bar("str", 5), Bar("str2", 6)),
      )
      val bar2 = FooBars(List(), List())

      val string1 = """<?xml version='1.0' encoding='UTF-8'?>
                      | <foobars>
                      |   <foo b="b value">
                      |     <a>1</a>
                      |     <c>3.0</c>
                      |   </foo>
                      |   <bar>
                      |     <b>5</b>
                      |     <a>str</a>
                      |   </bar>
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
                      |   <bar>
                      |     <a>str2</a>
                      |     <b>6</b>
                      |   </bar>
                      | </foobars>
                    """.stripMargin

      val string2 =
        """<?xml version='1.0' encoding='UTF-8'?>
          | <foobars/>
        """.stripMargin

      val decoded1 = XmlDecoder[FooBars].decodeFromIterable(toList(string1))
      val decoded2 = XmlDecoder[FooBars].decodeFromIterable(toList(string2))
      assert(decoded1 == Right(bar1) && decoded2 == Right(bar2))
    }

    "decode mixed lists sync" in decodeMixedLists(pure)
    "decode mixed lists async" in decodeMixedLists(fromIterable)

    def decodeByteArrays(toList: String => List[Array[Byte]]): Assertion = {
      case class Foo(@text content: Array[Byte])

      implicit val xmlDecoder: XmlDecoder[Foo] = deriveXmlDecoder("foo")

      val foo = Foo("foobar".getBytes)

      val string = """<?xml version='1.0' encoding='UTF-8'?>
                     | <foo>Zm9vYmFy</foo>
                    """.stripMargin

      val decoded = XmlDecoder[Foo].decodeFromIterable(toList(string))
      assert(decoded.map(d => java.util.Arrays.equals(d.content, foo.content)) == Right(true))
    }

    "decode byte arrays sync" in decodeByteArrays(pure)
    "decode byte arrays async" in decodeByteArrays(fromIterable)

    def decodeTextValues(toList: String => List[Array[Byte]]): Assertion = {
      case class Foo(@attr a: Int, @attr b: String, @text c: Double)
      case class Bar(d: String, foo: Foo, e: Char)

      implicit val fooDecoder: ElementDecoder[Foo] = deriveElementDecoder
      implicit val xmlDecoder: XmlDecoder[Bar]     = deriveXmlDecoder("bar")

      val bar = Bar("d value", Foo(1, "b value", 3.0), 'e')
      val string = """<?xml version='1.0' encoding='UTF-8'?>
                     | <bar>
                     |   <d>d value</d>
                     |   <foo a="1" b="b value">3.0</foo>
                     |   <e>e</e>
                     | </bar>
                   """.stripMargin

      val decoded = XmlDecoder[Bar].decodeFromIterable(toList(string))
      assert(decoded == Right(bar))
    }

    "decode text values sync" in decodeTextValues(pure)
    "decode text values async" in decodeTextValues(fromIterable)

    object decodeRecursiveValuesClasses {
      case class Foo(foo: Option[Foo], das: Int)

      implicit val fooDecoder: ElementDecoder[Foo] = deriveElementDecoder[Foo]
      implicit val xmlDecoder: XmlDecoder[Foo]     = XmlDecoder.fromElementDecoder("foo")
    }

    def decodeRecursiveValues(toList: String => List[Array[Byte]]): Assertion = {
      import decodeRecursiveValuesClasses._

      val foo = Foo(Some(Foo(Some(Foo(Some(Foo(Some(Foo(None, 4)), 3)), 2)), 1)), 0)
      val string = """<?xml version='1.0' encoding='UTF-8'?>
                     |<foo>
                     |   <foo>
                     |       <foo>
                     |           <foo>
                     |              <foo>
                     |                <das>4</das>
                     |              </foo>
                     |              <das>3</das>
                     |           </foo>
                     |           <das>2</das>
                     |       </foo>
                     |       <das>1</das>
                     |   </foo>
                     |   <das>0</das>
                     |</foo>
                   """.stripMargin

      val decoded = XmlDecoder[Foo].decodeFromIterable(toList(string))
      assert(decoded == Right(foo))
    }

    "decode recursive values sync" in decodeRecursiveValues(pure)
    "decode recursive values async" in decodeRecursiveValues(fromIterable)

    def ignoreExtraElements(toList: String => List[Array[Byte]]): Assertion = {
      case class Foo(das: Int)

      implicit val xmlDecoder: XmlDecoder[Foo] = deriveXmlDecoder("foo")

      val foo = Foo(0)
      val string = """<?xml version='1.0' encoding='UTF-8'?>
                     |<foo>
                     |   <foo>
                     |       <foo>
                     |           <foo>
                     |              <foo>
                     |                <das>4</das>
                     |              </foo>
                     |              <das>3</das>
                     |           </foo>
                     |           <das>2</das>
                     |       </foo>
                     |       <das>1</das>
                     |   </foo>
                     |   <das>0</das>
                     |</foo>
                   """.stripMargin

      val decoded = XmlDecoder[Foo].decodeFromIterable(toList(string))
      assert(decoded == Right(foo))

    }

    "ignore extra elements sync" in ignoreExtraElements(pure)
    "ignore extra elements async" in ignoreExtraElements(fromIterable)

    def decodeMixedContent(toList: String => List[Array[Byte]]): Assertion = {
      case class Foo(count: Int, buz: String, @text text: String)

      implicit val xmlDecoder: XmlDecoder[Foo] = deriveXmlDecoder("foo")

      val foo = Foo(1, "Buzz", "Sending  item to ")
      val string = """<?xml version='1.0' encoding='UTF-8'?>
                     |<foo>Sending <count>1</count> item to <buz>Buzz</buz></foo>
                   """.stripMargin

      val decoded = XmlDecoder[Foo].decodeFromIterable(toList(string))
      assert(decoded == Right(foo))

    }

    "decode mixed content sync" in decodeMixedContent(pure)
    "decode mixed content async" in decodeMixedContent(fromIterable)

    def decodeMixedContentForElementsWithSameName(toList: String => List[Array[Byte]]): Assertion = {
      case class Element2(@text text: String)
      case class Element1(element: Element2, @text text: String)
      case class Document(element: Element1)

      implicit val element2decoder: ElementDecoder[Element2] = deriveElementDecoder
      implicit val element1decoder: ElementDecoder[Element1] = deriveElementDecoder
      implicit val xmlDecoder: XmlDecoder[Document]          = deriveXmlDecoder("document")

      val string = """<?xml version='1.0' encoding='UTF-8'?>
                     |<document><element>text1<element>text2</element></element></document>
                   """.stripMargin

      val decoded = XmlDecoder[Document].decodeFromIterable(toList(string))

      assert(
        decoded == Right(Document(Element1(Element2("text2"), "text1"))),
      )
    }

    "decode mixed content for elements with same name sync" in decodeMixedContentForElementsWithSameName(pure)
    "decode mixed content for elements with same name async" in decodeMixedContentForElementsWithSameName(fromIterable)

    def escapeCharacters(toList: String => List[Array[Byte]]): Assertion = {
      case class Foo(@attr baz: String, bar: String)

      implicit val xmlDecoder: XmlDecoder[Foo] = deriveXmlDecoder("foo")

      val foo = Foo("Esca\"'<>&pe", "Esca\"'<>&pe")
      val string = """<?xml version='1.0' encoding='UTF-8'?>
                     |<foo baz="Esca&quot;&apos;&lt;>&amp;pe">
                     |  <bar>Esca"'&lt;>&amp;pe</bar>
                     |</foo>
                   """.stripMargin
      val decoded = XmlDecoder[Foo].decodeFromIterable(toList(string))
      assert(decoded == Right(foo))
    }

    "escape characters sync" in escapeCharacters(pure)
    "escape characters async" in escapeCharacters(fromIterable)

    def decodeWithRenamed(toList: String => List[Array[Byte]]): Assertion = {
      case class Foo(@attr baz: String, bar: String, @renamed("foooo") sample: String)

      implicit val xmlDecoder: XmlDecoder[Foo] = deriveXmlDecoder("foo")

      val foo = Foo("Esca\"'<>&pe", "Esca\"'<>&pe", "somefoo")
      val string = """<?xml version='1.0' encoding='UTF-8'?>
                     |<foo baz="Esca&quot;&apos;&lt;>&amp;pe">
                     |  <bar>Esca"'&lt;>&amp;pe</bar>
                     |  <foooo>somefoo</foooo>
                     |</foo>
                   """.stripMargin
      val decoded = XmlDecoder[Foo].decodeFromIterable(toList(string))
      assert(decoded == Right(foo))
    }

    "decodes with @renamed sync" in decodeWithRenamed(pure)
    "decodes with @renamed async" in decodeWithRenamed(fromIterable)

    def decodeWithRenamedNested(toList: String => List[Array[Byte]]): Assertion = {
      case class Foo(@attr baz: String, @renamed("theBar") bar: Bar)
      case class Bar(a: String, b: String)

      implicit val barDecoder: ElementDecoder[Bar] = deriveElementDecoder
      implicit val fooXmlDecoder: XmlDecoder[Foo]  = deriveXmlDecoder("foo")

      val foo = Foo("Esca\"'<>&pe", Bar("theA", "theB"))
      val string = """<?xml version='1.0' encoding='UTF-8'?>
                     |<foo baz="Esca&quot;&apos;&lt;>&amp;pe">
                     |  <theBar>
                     |    <a>theA</a>
                     |    <b>theB</b>
                     |  </theBar>
                     |</foo>
                   """.stripMargin
      val decoded = XmlDecoder[Foo].decodeFromIterable(toList(string))
      assert(decoded == Right(foo))
    }

    "decodes with @renamed nested sync" in decodeWithRenamedNested(pure)
    "decodes with @renamed nested async" in decodeWithRenamedNested(fromIterable)

    def decodeWithRenamedAttribute(toList: String => List[Array[Byte]]): Assertion = {
      case class Foo(@attr @renamed("theBaz") baz: String, bar: String)

      implicit val xmlDecoder: XmlDecoder[Foo] = deriveXmlDecoder("foo")

      val foo = Foo("Esca\"'<>&pe", "Esca\"'<>&pe")
      val string = """<?xml version='1.0' encoding='UTF-8'?>
                     |<foo theBaz="Esca&quot;&apos;&lt;>&amp;pe">
                     |  <bar>Esca"'&lt;>&amp;pe</bar>
                     |</foo>
                   """.stripMargin
      val decoded = XmlDecoder[Foo].decodeFromIterable(toList(string))
      assert(decoded == Right(foo))
    }

    "decodes with @renamed @attr sync" in decodeWithRenamedAttribute(pure)
    "decodes with @renamed @attr async" in decodeWithRenamedAttribute(fromIterable)

    def decodeRenamedTextValues(toList: String => List[Array[Byte]]): Assertion = {
      case class Foo(@attr a: Int, @attr @renamed("theB") b: String, @text c: Double)
      case class Bar(d: String, foo: Foo, e: Char)

      implicit val fooDecoder: ElementDecoder[Foo] = deriveElementDecoder
      implicit val xmlDecoder: XmlDecoder[Bar]     = deriveXmlDecoder("bar")

      val bar = Bar("d value", Foo(1, "b value", 3.0), 'e')
      val string = """<?xml version='1.0' encoding='UTF-8'?>
                     | <bar>
                     |   <d>d value</d>
                     |   <foo a="1" theB="b value">3.0</foo>
                     |   <e>e</e>
                     | </bar>
                   """.stripMargin

      val decoded = XmlDecoder[Bar].decodeFromIterable(toList(string))
      assert(decoded == Right(bar))
    }

    "decode @renamed text values sync" in decodeRenamedTextValues(pure)
    "decode @renamed text values async" in decodeRenamedTextValues(fromIterable)

    def decodeCamelCase(toList: String => List[Array[Byte]]): Assertion = {
      val camelCaseConfig = ElementCodecConfig.default.withFieldNamesTransformed(camelCase)
      case class Foo(@attr someName: Int, @attr someOther: String, @text c: Double)
      case class Bar(someTopName: String, someFoo: Foo, e: Char)

      implicit val fooDecoder: ElementDecoder[Foo] = deriveElementDecoderConfigured(camelCaseConfig)
      implicit val xmlDecoder: XmlDecoder[Bar]     = deriveXmlDecoderConfigured("Bar", camelCaseConfig)

      val bar = Bar("d value", Foo(1, "b value", 3.0), 'e')
      val string = """<?xml version='1.0' encoding='UTF-8'?>
                     | <Bar>
                     |   <SomeTopName>d value</SomeTopName>
                     |   <SomeFoo SomeName="1" SomeOther="b value">3.0</SomeFoo>
                     |   <E>e</E>
                     | </Bar>
                   """.stripMargin

      val decoded = XmlDecoder[Bar].decodeFromIterable(toList(string))
      assert(decoded == Right(bar))
    }

    "decode CamelCase sync" in decodeCamelCase(pure)
    "decode CamelCase async" in decodeCamelCase(fromIterable)

    def decodeSnakeCase(toList: String => List[Array[Byte]]): Assertion = {
      val snakeCaseConfig = ElementCodecConfig.default.withFieldNamesTransformed(snakeCase)
      case class Foo(@attr someName: Int, @attr someOther: String, @text c: Double)
      case class Bar(someTopName: String, someFoo: Foo, e: Char)

      implicit val fooDecoder: ElementDecoder[Foo] = deriveElementDecoderConfigured(snakeCaseConfig)
      implicit val xmlDecoder: XmlDecoder[Bar]     = deriveXmlDecoderConfigured("bar", snakeCaseConfig)

      val bar = Bar("d value", Foo(1, "b value", 3.0), 'e')
      val string = """<?xml version='1.0' encoding='UTF-8'?>
                     | <bar>
                     |   <some_top_name>d value</some_top_name>
                     |   <some_foo some_name="1" some_other="b value">3.0</some_foo>
                     |   <e>e</e>
                     | </bar>
                   """.stripMargin

      val decoded = XmlDecoder[Bar].decodeFromIterable(toList(string))
      assert(decoded == Right(bar))
    }

    "decode snake_case sync" in decodeSnakeCase(pure)
    "decode snake_case async" in decodeSnakeCase(fromIterable)

    def decodeRenamedPriority(toList: String => List[Array[Byte]]): Assertion = {
      val snakeCaseConfig = ElementCodecConfig.default.withFieldNamesTransformed(snakeCase)
      case class Foo(@attr someName: Int, @attr @renamed("i-Have-priority") someOther: String, @text c: Double)
      case class Bar(someTopName: String, @renamed("Me2") someFoo: Foo, e: Char)

      implicit val fooDecoder: ElementDecoder[Foo] = deriveElementDecoderConfigured(snakeCaseConfig)
      implicit val xmlDecoder: XmlDecoder[Bar]     = deriveXmlDecoderConfigured("bar", snakeCaseConfig)

      val bar = Bar("d value", Foo(1, "b value", 3.0), 'e')
      val string = """<?xml version='1.0' encoding='UTF-8'?>
                     | <bar>
                     |   <some_top_name>d value</some_top_name>
                     |   <Me2 some_name="1" i-Have-priority="b value">3.0</Me2>
                     |   <e>e</e>
                     | </bar>
                   """.stripMargin

      val decoded = XmlDecoder[Bar].decodeFromIterable(toList(string))
      assert(decoded == Right(bar))
    }

    "decode with @renamed having priority over naming sync" in decodeRenamedPriority(pure)
    "decode with @renamed having priority over naming async" in decodeRenamedPriority(fromIterable)

    def decodeWithDefaultDecoders(toList: String => List[Array[Byte]]): Assertion = {
      case class Default(a: Option[Int], b: Option[String], c: Option[Double])
      case class Foo(a: Int, b: String, @default defaults: List[Default])

      implicit val defaultDecoder: ElementDecoder[Default] = deriveElementDecoder
      implicit val xmlDecoder: XmlDecoder[Foo]             = deriveXmlDecoder("foo")

      val foo =
        Foo(
          1,
          "b value",
          List(
            Default(Some(100), None, None),
            Default(None, Some("default string"), None),
            Default(None, None, Some(12.3)),
          ),
        )
      val string = """<?xml version='1.0' encoding='UTF-8'?>
                     |<foo>
                     |  <default1><a>100</a></default1>
                     |  <a>1</a>
                     |  <default2><b>default string</b></default2>
                     |  <b>b value</b>
                     |  <default3><c>12.3</c></default3>
                     |</foo>
                   """.stripMargin

      val decoded = XmlDecoder[Foo].decodeFromIterable(toList(string))
      assert(decoded == Right(foo))
    }

    "decode with default decoders sync" in decodeWithDefaultDecoders(pure)
    "decode with default decoders async" in decodeWithDefaultDecoders(fromIterable)
  }

  "Decoder derivation for sealed traits" should {
    def decodeSealedTraits(toList: String => List[Array[Byte]]): Assertion = {
      case class Bar(d: String, foo: SealedClasses.Foo, e: Char)

      implicit val elementDecoder: ElementDecoder[Bar] = deriveElementDecoder

      val bar1 = Bar("d value", SealedClasses.Foo1("string"), 'k')
      val bar2 = Bar("d value", SealedClasses.Foo2(1), 'e')
      val bar3 = Bar("another one value", SealedClasses.Foo3(1.1234), 'v')

      val barDecoder = XmlDecoder.fromElementDecoder[Bar]("bar")

      val string1 = """<?xml version='1.0' encoding='UTF-8'?>
                      | <bar>
                      |   <d>d value</d>
                      |   <foo xmlns:ans1="http://www.w3.org/2001/XMLSchema-instance" ans1:type="Foo1">
                      |     <a>string</a>
                      |   </foo>
                      |   <e>k</e>
                      | </bar>
                    """.stripMargin
      val string2 = """<?xml version='1.0' encoding='UTF-8'?>
                      | <bar>
                      |   <d>d value</d>
                      |   <foo xmlns:ans1="http://www.w3.org/2001/XMLSchema-instance" ans1:type="Foo2">
                      |     <b>1</b>
                      |   </foo>
                      |   <e>e</e>
                      | </bar>
                    """.stripMargin
      val string3 = """<?xml version='1.0' encoding='UTF-8'?>
                      | <bar>
                      |   <d>another one value</d>
                      |   <foo xmlns:ans1="http://www.w3.org/2001/XMLSchema-instance" ans1:type="Foo3">
                      |     <c>1.1234</c>
                      |   </foo>
                      |   <e>v</e>
                      | </bar>
                   """.stripMargin
      val string4 = """<?xml version='1.0' encoding='UTF-8'?>
                      | <bar>
                      |   <d>another one value</d>
                      |   <foo xmlns:ans1="http://www.w3.org/2001/XMLSchema-instance" ans1:type="Qux">
                      |   </foo>
                      |   <e>v</e>
                      | </bar>
                   """.stripMargin
      assert(
        barDecoder.decodeFromIterable(toList(string1)) == Right(bar1) &&
          barDecoder.decodeFromIterable(toList(string2)) == Right(bar2) &&
          barDecoder.decodeFromIterable(toList(string3)) == Right(bar3) &&
          barDecoder.decodeFromIterable(toList(string4)) ==
          Left(DecodingError("Unknown type discriminator value: 'Qux'", List("foo", "bar"))),
      )
    }

    "decode sealed traits sync" in decodeSealedTraits(pure)
    "decode sealed traits async" in decodeSealedTraits(fromIterable)

    def decodeSealedTraitsWithCustomDiscriminator(toList: String => List[Array[Byte]]): Assertion = {
      case class Qux(d: String, bar: SealedClasses.Bar, e: Char)

      implicit val quxElementDecoder: ElementDecoder[Qux] = deriveElementDecoder

      val qux1 = Qux("d value", SealedClasses.Bar1("string"), 'k')
      val qux2 = Qux("d value", SealedClasses.Bar2(1), 'e')
      val qux3 = Qux("another one value", SealedClasses.Bar3(1.1234), 'v')

      val quxDecoder = XmlDecoder.fromElementDecoder[Qux]("qux")

      val string1 = """<?xml version='1.0' encoding='UTF-8'?>
                      | <qux>
                      |   <d>d value</d>
                      |   <bar discriminator="Bar1">
                      |     <a>string</a>
                      |   </bar>
                      |   <e>k</e>
                      | </qux>
                    """.stripMargin
      val string2 = """<?xml version='1.0' encoding='UTF-8'?>
                      | <qux>
                      |   <d>d value</d>
                      |   <bar discriminator="Bar2">
                      |     <b>1</b>
                      |   </bar>
                      |   <e>e</e>
                      | </qux>
                    """.stripMargin
      val string3 = """<?xml version='1.0' encoding='UTF-8'?>
                      | <qux>
                      |   <d>another one value</d>
                      |   <bar discriminator="Bar3">
                      |     <c>1.1234</c>
                      |   </bar>
                      |   <e>v</e>
                      | </qux>
                    """.stripMargin

      assert(
        quxDecoder.decodeFromIterable(toList(string1)) == Right(qux1) &&
          quxDecoder.decodeFromIterable(toList(string2)) == Right(qux2) &&
          quxDecoder.decodeFromIterable(toList(string3)) == Right(qux3),
      )

      case class Quux(d: String, baz: SealedClasses.Baz, e: Char)

      implicit val quuxElementDecoder: ElementDecoder[Quux] = deriveElementDecoder

      val quux1 = Quux("d value", SealedClasses.Baz1("string"), 'k')
      val quux2 = Quux("d value", SealedClasses.Baz2(1), 'e')
      val quux3 = Quux("another one value", SealedClasses.Baz3(1.1234), 'v')

      val quuxDecoder = XmlDecoder.fromElementDecoder[Quux]("quux")

      val string4 = """<?xml version='1.0' encoding='UTF-8'?>
                      | <quux>
                      |   <d>d value</d>
                      |   <baz xmlns:ans1="https://tinkoff.ru" ans1:discriminator="Baz1">
                      |     <a>string</a>
                      |   </baz>
                      |   <e>k</e>
                      | </quux>
                    """.stripMargin
      val string5 = """<?xml version='1.0' encoding='UTF-8'?>
                      | <quux>
                      |   <d>d value</d>
                      |   <baz xmlns:ans1="https://tinkoff.ru" ans1:discriminator="Baz2">
                      |     <b>1</b>
                      |   </baz>
                      |   <e>e</e>
                      | </quux>
                    """.stripMargin
      val string6 = """<?xml version='1.0' encoding='UTF-8'?>
                      | <quux>
                      |   <d>another one value</d>
                      |   <baz xmlns:ans1="https://tinkoff.ru" ans1:discriminator="Baz3">
                      |     <c>1.1234</c>
                      |   </baz>
                      |   <e>v</e>
                      | </quux>
                    """.stripMargin

      assert(
        quuxDecoder.decodeFromIterable(toList(string4)) == Right(quux1) &&
          quuxDecoder.decodeFromIterable(toList(string5)) == Right(quux2) &&
          quuxDecoder.decodeFromIterable(toList(string6)) == Right(quux3),
      )
    }

    "decode sealed traits with custom discriminator sync" in decodeSealedTraitsWithCustomDiscriminator(pure)
    "decode sealed traits with custom discriminator async" in decodeSealedTraitsWithCustomDiscriminator(fromIterable)

    def decodeSealedTraitsWithConstructorNamesTransformed(toList: String => List[Array[Byte]]) = {
      val wolf = SealedClasses.CanisLupus("Igor", 0.2, 20)
      val lion = SealedClasses.PantheraLeo("Sergey", 0.75, 60.1)

      val animalDecoder = XmlDecoder.fromElementDecoder[SealedClasses.Mammalia]("animal")

      val string1 = """<?xml version='1.0' encoding='UTF-8'?>
                      | <animal xmlns:ans1="http://www.w3.org/2001/XMLSchema-instance" ans1:type="canis_lupus">
                      |   <name>Igor</name>
                      |   <strength>0.2</strength>
                      |   <age>20</age>
                      | </animal>
                    """.stripMargin

      val string2 = """<?xml version='1.0' encoding='UTF-8'?>
                      | <animal xmlns:ans1="http://www.w3.org/2001/XMLSchema-instance" ans1:type="panthera_leo">
                      |   <name>Sergey</name>
                      |   <strength>0.75</strength>
                      |   <speed>60.1</speed>
                      | </animal>
                    """.stripMargin

      assert(
        animalDecoder.decodeFromIterable(toList(string1)) == Right(wolf) &&
          animalDecoder.decodeFromIterable(toList(string2)) == Right(lion),
      )
    }

    "decode sealed traits with constructor names transformed sync" in
      decodeSealedTraitsWithConstructorNamesTransformed(pure)
    "decode sealed traits with constructor names transformed async" in
      decodeSealedTraitsWithConstructorNamesTransformed(fromIterable)

    def decodeSealedTraitsWithCustomDiscriminatorValues(toList: String => List[Array[Byte]]): Assertion = {
      val hornet    = SealedClasses.Vespa("Anton", 200.123)
      val cockroach = SealedClasses.Blattodea("Dmitriy", 5)

      val insectDecoder = XmlDecoder.fromElementDecoder[SealedClasses.Insecta]("insect")

      val string1 = """<?xml version='1.0' encoding='UTF-8'?>
                      | <insect xmlns:ans1="http://www.w3.org/2001/XMLSchema-instance" ans1:type="hornet">
                      |   <name>Anton</name>
                      |   <damage>200.123</damage>
                      | </insect>
                    """.stripMargin

      val string2 = """<?xml version='1.0' encoding='UTF-8'?>
                      | <insect xmlns:ans1="http://www.w3.org/2001/XMLSchema-instance" ans1:type="cockroach">
                      |   <name>Dmitriy</name>
                      |   <legsNumber>5</legsNumber>
                      | </insect>
                    """.stripMargin

      assert(
        insectDecoder.decodeFromIterable(toList(string1)) == Right(hornet) &&
          insectDecoder.decodeFromIterable(toList(string2)) == Right(cockroach),
      )
    }

    "decode sealed traits with custom discriminator values sync" in
      decodeSealedTraitsWithCustomDiscriminatorValues(pure)
    "decode sealed traits with custom discriminator values async" in
      decodeSealedTraitsWithCustomDiscriminatorValues(fromIterable)

    def notTransformCustomDiscriminatorValues(toList: String => List[Array[Byte]]): Assertion = {
      val clownFish  = SealedClasses.Amphiprion("Nemo", 1)
      val whiteShark = SealedClasses.CarcharodonCarcharias("Bill", 20000000000L)

      val fishDecoder = XmlDecoder.fromElementDecoder[SealedClasses.Pisces]("fish")

      val string1 = """<?xml version='1.0' encoding='UTF-8'?>
                        | <fish xmlns:ans1="http://www.w3.org/2001/XMLSchema-instance" ans1:type="ClownFish">
                        |   <name>Nemo</name>
                        |   <finNumber>1</finNumber>
                        | </fish>
                      """.stripMargin

      val string2 = """<?xml version='1.0' encoding='UTF-8'?>
                        | <fish xmlns:ans1="http://www.w3.org/2001/XMLSchema-instance" ans1:type="carcharodon_carcharias">
                        |   <name>Bill</name>
                        |   <teethNumber>20000000000</teethNumber>
                        | </fish>
                      """.stripMargin

      assert(
        fishDecoder.decodeFromIterable(toList(string1)) == Right(clownFish) &&
          fishDecoder.decodeFromIterable(toList(string2)) == Right(whiteShark),
      )
    }
    "not transform custom discriminator values sync" in
      notTransformCustomDiscriminatorValues(pure)
    "not transform custom discriminator values async" in
      notTransformCustomDiscriminatorValues(fromIterable)

    def useElementNameAsDiscriminatorIfConfigured(toList: String => List[Array[Byte]]): Assertion = {
      case class Zoo(@default animals: List[Animal])

      implicit val xmlDecoder: XmlDecoder[Zoo] = deriveXmlDecoder("zoo")

      val string = """<?xml version='1.0' encoding='UTF-8'?>
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
                    """.stripMargin
      val zoo = Zoo(List(Cow(12.432), Cat("meow"), Dog(1234), Cat("nya")))
      XmlDecoder[Zoo].decodeFromIterable(toList(string)) shouldBe Right(zoo)
    }

    "use element name as discriminator if configured sync" in useElementNameAsDiscriminatorIfConfigured(pure)
    "use element name as discriminator if configured async" in useElementNameAsDiscriminatorIfConfigured(fromIterable)

    def overrideElementNameWithDiscriminatorInXmlDecoderIfConfigured(toList: String => List[Array[Byte]]): Assertion = {
      // "animal" must be ignored if useElementNamesAsDiscriminator is set to true
      val animalXmlDecoder: XmlDecoder[Animal] = XmlDecoder.fromElementDecoder("animal")
      val catString =
        """<?xml version='1.0' encoding='UTF-8'?>
          |<cat>
          |  <meow>meow</meow>
          |</cat>
          |""".stripMargin
      val dogString =
        """<?xml version='1.0' encoding='UTF-8'?>
          |<dog>
          |  <woof>1234</woof>
          |</dog>
          |""".stripMargin
      val robotString =
        """<?xml version='1.0' encoding='UTF-8'?>
          |<robot>
          |  <woof>1234</woof>
          |</robot>
          |""".stripMargin

      animalXmlDecoder.decode(catString) shouldBe Right(Cat("meow"))
      animalXmlDecoder.decode(dogString) shouldBe Right(Dog(1234))
      animalXmlDecoder.decode(robotString) shouldBe
        Left(DecodingError("Unknown type discriminator value: 'robot'", List("robot")))
    }

    "override element name with discriminator in xml decoder if configured sync" in
      overrideElementNameWithDiscriminatorInXmlDecoderIfConfigured(pure)
    "override element name with discriminator in xml decoder if configured async" in
      overrideElementNameWithDiscriminatorInXmlDecoderIfConfigured(fromIterable)
  }

  "Decoder derivation with namespaces" should {

    def decodeSimpleCaseClasses(toList: String => List[Array[Byte]]): Assertion = {
      object tkf
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

      implicit val fooDecoder: ElementDecoder[Foo] = deriveElementDecoder
      implicit val xmlDecoder: XmlDecoder[Bar]     = deriveXmlDecoder("bar", tkf)

      val bar = Bar("d value", Foo(1, "b value", 3.0), 'e')
      val string = """<?xml version='1.0' encoding='UTF-8'?>
                     | <ans1:bar xmlns:ans1="tinkoff.ru">
                     |   <ans1:d>d value</ans1:d>
                     |   <ans1:foo>
                     |     <ans1:a>1</ans1:a>
                     |     <ans1:b>b value</ans1:b>
                     |     <ans1:c>3.0</ans1:c>
                     |   </ans1:foo>
                     |   <ans1:e>e</ans1:e>
                     | </ans1:bar>
                   """.stripMargin

      val decoded = XmlDecoder[Bar].decodeFromIterable(toList(string))
      assert(decoded == Right(bar))
    }

    "decode simple case classes sync" in decodeSimpleCaseClasses(pure)
    "decode simple case classes async" in decodeSimpleCaseClasses(fromIterable)

    def decodeAttributes(toList: String => List[Array[Byte]]): Assertion = {
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

      implicit val fooDecoder: ElementDecoder[Foo] = deriveElementDecoder
      implicit val xmlDecoder: XmlDecoder[Bar]     = deriveXmlDecoder("bar", tkf)

      val bar = Bar("d value", Foo(1, "b value", 3.0), 'e')
      val string = """<?xml version='1.0' encoding='UTF-8'?>
                     | <ans1:bar xmlns:ans1="tinkoff.ru" ans1:e="e">
                     |   <ans1:d>d value</ans1:d>
                     |   <ans1:foo ans1:b="b value">
                     |     <ans1:a>1</ans1:a>
                     |     <ans1:c>3.0</ans1:c>
                     |   </ans1:foo>
                     | </ans1:bar>
                   """.stripMargin

      val decoded = XmlDecoder[Bar].decodeFromIterable(toList(string))
      assert(decoded == Right(bar))

    }

    "decode attributes sync" in decodeAttributes(pure)
    "decode attributes async" in decodeAttributes(fromIterable)

    def decodeNestedNamespaces(toList: String => List[Array[Byte]]): Assertion = {
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

      implicit val fooDecoder: ElementDecoder[Foo] = deriveElementDecoder
      implicit val xmlDecoder: XmlDecoder[Bar]     = deriveXmlDecoder("bar")

      val bar = Bar("d value", Foo(1, "b value", 3.0), 'e')
      val string = """<?xml version='1.0' encoding='UTF-8'?>
                     | <bar e="e">
                     |   <d>d value</d>
                     |   <ans1:foo xmlns:ans1="tinkoff.ru" b="b value">
                     |     <ans1:a>1</ans1:a>
                     |     <ans1:c>3.0</ans1:c>
                     |   </ans1:foo>
                     | </bar>
                   """.stripMargin
      val decoded = XmlDecoder[Bar].decodeFromIterable(toList(string))
      assert(decoded == Right(bar))

    }

    "decode nested namespaces sync" in decodeNestedNamespaces(pure)
    "decode nested namespaces async" in decodeNestedNamespaces(fromIterable)

    def decodeMultipleNamespaces(toList: String => List[Array[Byte]]): Assertion = {
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

      implicit val fooDecoder: ElementDecoder[Foo] = deriveElementDecoder
      implicit val xmlDecoder: XmlDecoder[Bar]     = deriveXmlDecoder("bar", tcs)

      val bar = Bar("d value", Foo(1, "b value", 3.0), 'e')
      val string = """<?xml version='1.0' encoding='UTF-8'?>
                     | <ans1:bar xmlns:ans1="tcsbank.ru" e="e">
                     |   <ans1:d>d value</ans1:d>
                     |   <ans2:foo xmlns:ans2="tinkoff.ru" b="b value">
                     |     <ans2:a>1</ans2:a>
                     |     <ans2:c>3.0</ans2:c>
                     |   </ans2:foo>
                     | </ans1:bar>
                   """.stripMargin

      val decoded = XmlDecoder[Bar].decodeFromIterable(toList(string))
      assert(decoded == Right(bar))

    }

    "decode multiple namespaces sync" in decodeMultipleNamespaces(pure)
    "decode multiple namespaces async" in decodeMultipleNamespaces(fromIterable)

    def decodeCamelCase(toList: String => List[Array[Byte]]): Assertion = {
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

      implicit val fooDecoder: ElementDecoder[Foo] = deriveElementDecoderConfigured(camelCaseConfig)
      implicit val xmlDecoder: XmlDecoder[Bar]     = deriveXmlDecoderConfigured("Bar", tkf, camelCaseConfig)

      val bar = Bar("d value", Foo(1, "b value", 3.0))
      val string = """<?xml version='1.0' encoding='UTF-8'?>
                     | <ans1:Bar xmlns:ans1="tinkoff.ru">
                     |   <ans1:SomeTopName>d value</ans1:SomeTopName>
                     |   <ans1:SomeFoo>
                     |     <ans1:SomeName>1</ans1:SomeName>
                     |     <ans1:SomeOtherName>b value</ans1:SomeOtherName>
                     |     <ans1:C>3.0</ans1:C>
                     |   </ans1:SomeFoo>
                     | </ans1:Bar>
                     """.stripMargin

      val decoded = XmlDecoder[Bar].decodeFromIterable(toList(string))
      assert(decoded == Right(bar))
    }

    "decode CamelCase sync" in decodeCamelCase(pure)
    "decode CamelCase async" in decodeCamelCase(fromIterable)

    def decodeSnakeCase(toList: String => List[Array[Byte]]): Assertion = {
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

      implicit val fooDecoder: ElementDecoder[Foo] = deriveElementDecoderConfigured(snakeCaseConfig)
      implicit val xmlDecoder: XmlDecoder[Bar]     = deriveXmlDecoderConfigured("bar", tkf, snakeCaseConfig)

      val bar = Bar("d value", Foo(1, "b value", 3.0))
      val string = """<?xml version='1.0' encoding='UTF-8'?>
                     | <ans1:bar xmlns:ans1="tinkoff.ru">
                     |   <ans1:some_top_name>d value</ans1:some_top_name>
                     |   <ans1:some_foo>
                     |     <ans1:some_name>1</ans1:some_name>
                     |     <ans1:some_other_name>b value</ans1:some_other_name>
                     |     <ans1:c>3.0</ans1:c>
                     |   </ans1:some_foo>
                     | </ans1:bar>
                     """.stripMargin

      val decoded = XmlDecoder[Bar].decodeFromIterable(toList(string))
      assert(decoded == Right(bar))
    }

    "decode snake_case sync" in decodeSnakeCase(pure)
    "decode snake_case async" in decodeSnakeCase(fromIterable)

    def decodeWithDefaultElementNamespaces(toList: String => List[Array[Byte]]): Assertion = {
      case object tkf
      implicit val tkfNs: Namespace[tkf.type] = Namespace.mkInstance("tinkoff.ru")

      val defaultNamespaceConfig = ElementCodecConfig.default.withElementsDefaultNamespace(tkf)
      case class Foo(a: Int, b: String, c: Double)
      case class Bar(@attr d: String, foo: Foo, e: Char)

      implicit val fooDecoder: ElementDecoder[Foo] = deriveElementDecoder
      implicit val xmlDecoder: XmlDecoder[Bar]     = deriveXmlDecoderConfigured("bar", tkf, defaultNamespaceConfig)

      val bar = Bar("d value", Foo(1, "b value", 3.0), 'e')
      val string =
        """<?xml version='1.0' encoding='UTF-8'?>
          | <ans1:bar xmlns:ans1="tinkoff.ru" d="d value">
          |   <ans1:foo>
          |     <a>1</a>
          |     <b>b value</b>
          |     <c>3.0</c>
          |   </ans1:foo>
          |  <ans1:e>e</ans1:e>
          | </ans1:bar>
        """.stripMargin
      val decoded = XmlDecoder[Bar].decodeFromIterable(toList(string))

      assert(decoded == Right(bar))
    }

    "decode with default element namespaces sync" in decodeWithDefaultElementNamespaces(pure)
    "decode with default element namespaces async" in decodeWithDefaultElementNamespaces(fromIterable)

    def overrideDefaultElementNamespaceWithNamespaceFromAnnotation(toList: String => List[Array[Byte]]): Assertion = {
      case object tkf
      implicit val tkfNs: Namespace[tkf.type] = Namespace.mkInstance("tinkoff.ru")
      case object tcs
      implicit val tcsNs: Namespace[tcs.type] = Namespace.mkInstance("tcsbank.ru")

      val defaultNamespaceConfig = ElementCodecConfig.default.withElementsDefaultNamespace(tkf)
      case class Foo(a: Int, b: String, c: Double)
      case class Bar(@attr d: String, @xmlns(tcs) foo: Foo, e: Char)

      implicit val fooDecoder: ElementDecoder[Foo] = deriveElementDecoder
      implicit val xmlDecoder: XmlDecoder[Bar]     = deriveXmlDecoderConfigured("bar", tkf, defaultNamespaceConfig)

      val bar = Bar("d value", Foo(1, "b value", 3.0), 'e')
      val string =
        """<?xml version='1.0' encoding='UTF-8'?>
          | <ans1:bar xmlns:ans1="tinkoff.ru" d="d value">
          |   <ans2:foo xmlns:ans2="tcsbank.ru">
          |     <a>1</a>
          |     <b>b value</b>
          |     <c>3.0</c>
          |   </ans2:foo>
          |  <ans1:e>e</ans1:e>
          | </ans1:bar>
        """.stripMargin

      val decoded = XmlDecoder[Bar].decodeFromIterable(toList(string))

      assert(decoded == Right(bar))
    }

    "override default element namespace with namespace from annotation sync" in
      overrideDefaultElementNamespaceWithNamespaceFromAnnotation(pure)
    "override default element namespace with namespace from annotation async" in
      overrideDefaultElementNamespaceWithNamespaceFromAnnotation(fromIterable)

    def decodeWithDefaultAttributeNamespaces(toList: String => List[Array[Byte]]): Assertion = {
      case object tkf
      implicit val tkfNs: Namespace[tkf.type] = Namespace.mkInstance("tinkoff.ru")

      val defaultNamespaceConfig = ElementCodecConfig.default.withAttributesDefaultNamespace(tkf)
      case class Foo(a: Int, b: String, c: Double)
      case class Bar(@attr d: String, foo: Foo, @attr e: Char)

      implicit val fooDecoder: ElementDecoder[Foo] = deriveElementDecoder
      implicit val xmlDecoder: XmlDecoder[Bar]     = deriveXmlDecoderConfigured("bar", tkf, defaultNamespaceConfig)

      val bar = Bar("d value", Foo(1, "b value", 3.0), 'e')
      val string =
        """<?xml version='1.0' encoding='UTF-8'?>
        | <ans1:bar xmlns:ans1="tinkoff.ru" ans1:d="d value" ans1:e="e">
        |   <foo>
        |     <a>1</a>
        |     <b>b value</b>
        |     <c>3.0</c>
        |   </foo>
        | </ans1:bar>
      """.stripMargin
      val decoded = XmlDecoder[Bar].decodeFromIterable(toList(string))

      assert(decoded == Right(bar))
    }

    "decode with default attribute namespaces sync" in decodeWithDefaultAttributeNamespaces(pure)
    "decode with default attribute namespaces async" in decodeWithDefaultAttributeNamespaces(fromIterable)

    def overrideDefaultAttributeNamespaceWithNamespaceFromAnnotation(toList: String => List[Array[Byte]]): Assertion = {
      case object tkf
      implicit val tkfNs: Namespace[tkf.type] = Namespace.mkInstance("tinkoff.ru")
      case object tcs
      implicit val tcsNs: Namespace[tcs.type] = Namespace.mkInstance("tcsbank.ru")

      val defaultNamespaceConfig = ElementCodecConfig.default.withAttributesDefaultNamespace(tkf)
      case class Foo(a: Int, b: String, c: Double)
      case class Bar(@attr d: String, foo: Foo, @attr @xmlns(tcs) e: Char)

      implicit val fooDecoder: ElementDecoder[Foo] = deriveElementDecoder
      implicit val xmlDecoder: XmlDecoder[Bar]     = deriveXmlDecoderConfigured("bar", tkf, defaultNamespaceConfig)

      val bar = Bar("d value", Foo(1, "b value", 3.0), 'e')
      val string =
        """<?xml version='1.0' encoding='UTF-8'?>
          | <ans1:bar xmlns:ans1="tinkoff.ru" ans1:d="d value" xmlns:ans2="tcsbank.ru" ans2:e="e">
          |   <foo>
          |     <a>1</a>
          |     <b>b value</b>
          |     <c>3.0</c>
          |   </foo>
          | </ans1:bar>
        """.stripMargin
      val decoded = XmlDecoder[Bar].decode(string)

      assert(decoded == Right(bar))
    }

    "override default attribute namespace with namespace from annotation sync" in
      overrideDefaultAttributeNamespaceWithNamespaceFromAnnotation(pure)
    "override default attribute namespace with namespace from annotation async" in
      overrideDefaultAttributeNamespaceWithNamespaceFromAnnotation(fromIterable)

    def failWhenElementNameIsIncorrect(toList: String => List[Array[Byte]]): Assertion = {
      case object tcs
      implicit val tcsNs: Namespace[tcs.type] = Namespace.mkInstance("tinkoff.ru")

      case class Foo(a: Int, b: String, c: Double)
      case class Bar(@attr d: String, foo: Foo, @attr e: Char)

      implicit val fooDecoder: ElementDecoder[Foo] = deriveElementDecoder
      implicit val xmlDecoder: XmlDecoder[Bar]     = deriveXmlDecoder("bar", tcs)

      val string1 =
        """<?xml version='1.0' encoding='UTF-8'?>
          | <ans1:wrong xmlns:ans1="tinkoff.ru" d="d value" e="e">
          |   <foo>
          |     <a>1</a>
          |     <b>b value</b>
          |     <c>3.0</c>
          |   </foo>
          | </ans1:wrong>
        """.stripMargin
      val string2 =
        """<?xml version='1.0' encoding='UTF-8'?>
          | <ans1:bar xmlns:ans1="tcsbank.ru" d="d value" e="e">
          |   <foo>
          |     <a>1</a>
          |     <b>b value</b>
          |     <c>3.0</c>
          |   </foo>
          | </ans1:bar>
        """.stripMargin
      val decoded1 = XmlDecoder[Bar].decode(string1)
      val decoded2 = XmlDecoder[Bar].decode(string2)

      assert(
        decoded1 == Left(DecodingError("Invalid local name. Expected 'bar', but found 'wrong'", List("wrong"))) &&
          decoded2 == Left(
            DecodingError("Invalid namespace. Expected 'tinkoff.ru', but found 'tcsbank.ru'", List("bar")),
          ),
      )
    }

    "fail when element name is incorrect sync" in
      failWhenElementNameIsIncorrect(pure)
    "fail when element name is incorrect async" in
      failWhenElementNameIsIncorrect(fromIterable)

    def decodeSealedTraits(toList: String => List[Array[Byte]]): Assertion = {
      case object tkf
      implicit val tkfNs: Namespace[tkf.type] = Namespace.mkInstance("tinkoff.ru")

      case class Aquarium(@xmlns(tkf) fish: List[SealedClasses.Pisces])
      implicit val xmlDecoder: XmlDecoder[Aquarium] =
        deriveXmlDecoderConfigured("aquarium", ElementCodecConfig.default.withNamespaceDefined(tkf))
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
          |""".stripMargin
      val aquarium =
        Aquarium(List(SealedClasses.Amphiprion("Marlin", 3), SealedClasses.CarcharodonCarcharias("Jaws", 1234)))
      XmlDecoder[Aquarium].decodeFromIterable(toList(string)) shouldBe Right(aquarium)
    }

    "decode sealed traits sync" in decodeSealedTraits(pure)
    "decode sealed traits async" in decodeSealedTraits(fromIterable)

    def decodeSealedTraitsUsingElementNamesAsDiscriminators(toList: String => List[Array[Byte]]): Assertion = {
      case object tkf
      implicit val tkfNs: Namespace[tkf.type] = Namespace.mkInstance("tinkoff.ru")

      case class AnimalShelter(@xmlns(tkf) @default animals: List[SealedClasses.Animal])
      implicit val xmlDecoder: XmlDecoder[AnimalShelter] =
        deriveXmlDecoderConfigured("shelter", ElementCodecConfig.default.withNamespaceDefined(tkf))

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
        """.stripMargin
      val animalShelter = AnimalShelter(List(Cat("meow"), Dog(1234)))
      XmlDecoder[AnimalShelter].decodeFromIterable(toList(string)) shouldBe Right(animalShelter)
    }

    "decode sealed traits using element names as discriminators sync" in
      decodeSealedTraitsUsingElementNamesAsDiscriminators(pure)
    "decode sealed traits using element names as discriminators async" in
      decodeSealedTraitsUsingElementNamesAsDiscriminators(fromIterable)

    def decodeElementsWithScopeNamespaceFromConfig(toList: String => List[Array[Byte]]): Assertion = {
      case object tkf
      implicit val tkfNs: Namespace[tkf.type] = Namespace.mkInstance("tinkoff.ru")

      case object tcs
      implicit val tcsNs: Namespace[tcs.type] = Namespace.mkInstance("tcsbank.ru")

      final case class Qux(g: String, h: Int)
      implicit val quxDecoder: ElementDecoder[Qux] = deriveElementDecoder

      final case class Foo(
          d: Int,
          @xmlns(tcs) e: String,
          f: Double,
          qux: List[Qux]
      )
      implicit val fooEncoder: ElementDecoder[Foo] = deriveElementDecoder

      final case class Bar(@xmlns(tcs) a: Int, b: String, c: Double, foo: Foo)
      val config                               = ElementCodecConfig.default.withScopeDefaultNamespace(tkf)
      implicit val barEncoder: XmlDecoder[Bar] = deriveXmlDecoderConfigured("bar", config)

      val bar = Bar(123, "b value", 1.234, Foo(321, "e value", 4.321, List(Qux("g value 1", 1), Qux("g value 2", 2))))
      val string1 =
        """<?xml version='1.0' encoding='UTF-8'?>
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
          |""".stripMargin

      val string2 =
        """<?xml version='1.0' encoding='UTF-8'?>
          | <ans1:bar xmlns:ans1="tinkoff.ru" xmlns:ans2="tcsbank.ru">
          |   <ans2:a>123</ans2:a>
          |   <ans1:b>b value</ans1:b>
          |   <ans1:c>1.234</ans1:c>
          |   <ans1:foo>
          |     <ans1:d>321</ans1:d>
          |     <ans2:e>e value</ans2:e>
          |     <ans1:f>4.321</ans1:f>
          |     <ans1:qux>
          |       <ans1:g>g value 1</ans1:g>
          |       <ans1:h>1</ans1:h>
          |     </ans1:qux>
          |     <ans1:qux>
          |       <ans1:g>g value 2</ans1:g>
          |       <ans1:h>2</ans1:h>
          |     </ans1:qux>
          |   </ans1:foo>
          | </ans1:bar>
          |""".stripMargin

      XmlDecoder[Bar].decode(string1) shouldBe Right(bar)
      XmlDecoder[Bar].decode(string2) shouldBe Right(bar)
    }

    "decode elements with scope namespace from config sync" in decodeElementsWithScopeNamespaceFromConfig(pure)
    "decode elements with scope namespace from config async" in decodeElementsWithScopeNamespaceFromConfig(fromIterable)

    def decodeElementsWithNestedScopeNamespacesFromConfig(toList: String => List[Array[Byte]]): Assertion = {
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
      implicit val fooEncoder: ElementDecoder[Foo] = deriveElementDecoderConfigured(fooConfig)

      final case class Bar(@xmlns(tcs) a: Int, b: String, c: Double, foo: Foo)
      val barConfig                            = ElementCodecConfig.default.withScopeDefaultNamespace(tkf)
      implicit val barEncoder: XmlDecoder[Bar] = deriveXmlDecoderConfigured("bar", barConfig)

      val bar = Bar(123, "b value", 1.234, Foo(321, "e value", 4.321))
      val string1 =
        """<?xml version='1.0' encoding='UTF-8'?>
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
          |""".stripMargin

      val string2 =
        """<?xml version='1.0' encoding='UTF-8'?>
          | <ans1:bar xmlns:ans1="tinkoff.ru" xmlns:ans2="tcsbank.ru">
          |   <ans2:a>123</ans2:a>
          |   <ans1:b>b value</ans1:b>
          |   <ans1:c>1.234</ans1:c>
          |   <ans3:foo xmlns:ans3="example.org">
          |     <ans3:d>321</ans3:d>
          |     <ans2:e>e value</ans2:e>
          |     <ans3:f>4.321</ans3:f>
          |   </ans3:foo>
          | </ans1:bar>
          |""".stripMargin

      XmlDecoder[Bar].decode(string1) shouldBe Right(bar)
      XmlDecoder[Bar].decode(string2) shouldBe Right(bar)
    }

    "decode elements with nested scope namespaces from config sync" in
      decodeElementsWithNestedScopeNamespacesFromConfig(pure)
    "decode elements with nested scope namespaces from config async" in
      decodeElementsWithNestedScopeNamespacesFromConfig(fromIterable)
  }

  "Decoder derivation compilation" should {
    "fail if wrong attributes" in {
      """
        | case class NotAttribute(a: Int)
        | implicit val notAttributeDecoder: ElementDecoder[NotAttribute] = deriveElementDecoder
        | case class Wrapper(@attr attribute: NotAttribute)
        | implicit val wrapperDecoder: ElementDecoder[Wrapper] = deriveElementDecoder
      """.stripMargin shouldNot typeCheck
    }

    "fail if wrong texts" in {
      """
        | case class NotText(a: Int)
        | implicit val notTextDecoder: ElementDecoder[NotText] = deriveElementDecoder
        | case class Wrapper(@text text: NotText, @attr a: Int)
        | implicit val wrapperDecoder: ElementDecoder[Wrapper] = deriveElementDecoder
      """.stripMargin shouldNot typeCheck
    }

    "fail if multiple xml annotations" in {
      """
        | case class AttrText(@attr @text attrText: Int, b: String)
        | implicit val attrTextDecoder: ElementDecoder[AttrText] = deriveElementDecoder
      """.stripMargin shouldNot typeCheck
    }

    "fail if multiple texts" in {
      """
        | case class MultiText(@text a: Int, @text b: String)
        | implicit val multiTextDecoder: ElementDecoder[MultiText] = deriveElementDecoder
      """.stripMargin shouldNot typeCheck
    }
  }
}
