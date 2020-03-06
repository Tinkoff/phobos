package ru.tinkoff.phobos

import cats.syntax.option._
import cats.instances.list._
import org.scalatest._
import ru.tinkoff.phobos.annotations.{ElementCodec, XmlCodec, XmlCodecNs, XmlnsDef}
import ru.tinkoff.phobos.decoding.{AttributeDecoder, ElementDecoder, TextDecoder, XmlDecoder}
import ru.tinkoff.phobos.syntax._
import ru.tinkoff.phobos.configured.naming._
import ru.tinkoff.phobos.configured.ElementCodecConfig

class DecoderDerivationSuit extends WordSpec with Matchers {
  def pure(str: String): List[Array[Byte]] =
    List(str.getBytes("UTF-8"))

  def fromIterable(str: String): List[Array[Byte]] =
    str.toList.map(c => Array(c.toByte))

  "Decoder derivation without namespaces" should {

    def decodeSimpleCaseClasses(toList: String => List[Array[Byte]]): Assertion = {
      @ElementCodec
      case class Foo(a: Int, b: String, c: Double)
      @XmlCodec("bar")
      case class Bar(d: String, foo: Foo, e: Char)

      val bar    = Bar("d value", Foo(1, "b value", 3.0), 'e')
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

      val decoded = XmlDecoder[Bar].decodeFromFoldable(toList(string))
      assert(decoded == Right(bar))
    }

    "decode simple case classes sync" in decodeSimpleCaseClasses(pure)
    "decode simple case classes async" in decodeSimpleCaseClasses(fromIterable)

    def decodeAttributes(toList: String => List[Array[Byte]]): Assertion = {
      @ElementCodec
      case class Foo(a: Int, @attr b: String, c: Double)
      @XmlCodec("bar")
      case class Bar(d: String, foo: Foo, @attr e: Char)

      val bar = Bar("d value", Foo(1, "b value", 3.0), 'e')

      val string  = """<?xml version='1.0' encoding='UTF-8'?>
                     | <bar e="e">
                     |   <d>d value</d>
                     |   <foo b="b value">
                     |     <a>1</a>
                     |     <c>3.0</c>
                     |   </foo>
                     | </bar>
                   """.stripMargin
      val decoded = XmlDecoder[Bar].decodeFromFoldable(toList(string))
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

      @ElementCodec
      case class Foo(@attr bar: Int, @text baz: Double)
      @XmlCodec("qux")
      case class Qux(str: String, foo: Foo)

      val qux = Qux("constant", Foo(24, -42.0))
      val string =
        """<?xml version='1.0' encoding='UTF-8'?>
          | <qux>
          |   <str>Not that constant</str>
          |   <foo bar="not number">not number</foo>
          | </qux>
          """.stripMargin
      val decoded = XmlDecoder[Qux].decodeFromFoldable(toList(string))
      assert(decoded == Right(qux))

    }

    "allow to override codecs sync" in allowToOverrideCodecs(pure)
    "allow to override codecs async" in allowToOverrideCodecs(fromIterable)

    def decodeOptions(toList: String => List[Array[Byte]]): Assertion = {
      @ElementCodec
      case class Foo(a: Option[Int], @attr b: String, c: Option[Double])
      @XmlCodec("Wrapper")
      case class Wrapper(foo: Option[Foo])

      val opt1 = Wrapper(Some(Foo(Some(1), "b", Some(2.0))))
      val opt2 = Wrapper(None)
      val opt3 = Wrapper(Some(Foo(None, "b", None)))

      val string1  = """<?xml version='1.0' encoding='UTF-8'?>
                      | <Wrapper>
                      |   <foo b="b">
                      |     <a>1</a>
                      |     <c>2.0</c>
                      |   </foo>
                      | </Wrapper>
                    """.stripMargin
      val string2  = """<?xml version='1.0' encoding='UTF-8'?>
                      | <Wrapper/>
                    """.stripMargin
      val string3  = """<?xml version='1.0' encoding='UTF-8'?>
                       | <Wrapper>
                       |   <foo b="b"/>
                       | </Wrapper>
                    """.stripMargin
      val string4  = """<?xml version='1.0' encoding='UTF-8'?>
                       | <Wrapper>
                       |   <foo/>
                       | </Wrapper>
                    """.stripMargin
      val decoded1 = XmlDecoder[Wrapper].decodeFromFoldable(toList(string1))
      val decoded2 = XmlDecoder[Wrapper].decodeFromFoldable(toList(string2))
      val decoded3 = XmlDecoder[Wrapper].decodeFromFoldable(toList(string3))
      val decoded4 = XmlDecoder[Wrapper].decodeFromFoldable(toList(string4))
      assert(decoded1 == Right(opt1) && decoded2 == Right(opt2) && decoded3 == Right(opt3) && decoded4 == Right(opt2))
    }

    "decode options sync" in decodeOptions(pure)
    "decode options async" in decodeOptions(fromIterable)

    def decodeNilValues(toList: String => List[Array[Byte]]): Assertion = {
      @ElementCodec
      case class Foo(a: Int, @attr b: String)
      @XmlCodec("Wrapper1")
      case class Wrapper1(foo: List[Foo])
      @XmlCodec("Wrapper2")
      case class Wrapper2(foo: Option[Foo])

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

      val decoded1 = XmlDecoder[Wrapper1].decodeFromFoldable(toList(string1))
      val decoded2 = XmlDecoder[Wrapper2].decodeFromFoldable(toList(string2))
      assert(decoded1 == Right(wrapper1) && decoded2 == Right(wrapper2))
    }

    "decode nil values sync" in decodeNilValues(pure)
    "decode nil values async" in decodeNilValues(fromIterable)

    def decodeLists(toList: String => List[Array[Byte]]): Assertion = {
      @ElementCodec
      case class Foo(a: Int, @attr b: Option[String], c: Option[Double])
      @XmlCodec("foos")
      case class Foos(foo: List[Foo])
      val bar1 = Foos(
        List(Foo(1, "b value".some, 3.0.some),
             Foo(2, None, 4.0.some),
             Foo(3, "It's three".some, None),
             Foo(4, None, None)))
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

      val decoded1 = XmlDecoder[Foos].decodeFromFoldable(toList(string1))
      val decoded2 = XmlDecoder[Foos].decodeFromFoldable(toList(string2))
      assert(decoded1 == Right(bar1) && decoded2 == Right(bar2))
    }

    "decode lists sync" in decodeLists(pure)
    "decode lists async" in decodeLists(fromIterable)

    def decodeMixedLists(toList: String => List[Array[Byte]]): Assertion = {
      @ElementCodec
      case class Foo(a: Int, @attr b: Option[String], c: Option[Double])
      @ElementCodec
      case class Bar(a: String, b: Int)
      @XmlCodec("foobars")
      case class FooBars(foo: List[Foo], bar: List[Bar])

      val bar1 = FooBars(
        List(Foo(1, "b value".some, 3.0.some),
             Foo(2, None, 4.0.some),
             Foo(3, "It's three".some, None),
             Foo(4, None, None)),
        List(Bar("str", 5), Bar("str2", 6))
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

      val decoded1 = XmlDecoder[FooBars].decodeFromFoldable(toList(string1))
      val decoded2 = XmlDecoder[FooBars].decodeFromFoldable(toList(string2))
      assert(decoded1 == Right(bar1) && decoded2 == Right(bar2))
    }

    "decode mixed lists sync" in decodeMixedLists(pure)
    "decode mixed lists async" in decodeMixedLists(fromIterable)

    def decodeByteArrays(toList: String => List[Array[Byte]]): Assertion = {
      @XmlCodec("foo")
      case class Foo(@text content: Array[Byte])
      val foo = Foo("foobar".getBytes)

      val string = """<?xml version='1.0' encoding='UTF-8'?>
                     | <foo>Zm9vYmFy</foo>
                    """.stripMargin

      val decoded = XmlDecoder[Foo].decodeFromFoldable(toList(string))
      assert(decoded.map(d => java.util.Arrays.equals(d.content, foo.content)) == Right(true))
    }

    "decode byte arrays sync" in decodeByteArrays(pure)
    "decode byte arrays async" in decodeByteArrays(fromIterable)

    def decodeTextValues(toList: String => List[Array[Byte]]): Assertion = {
      @ElementCodec
      case class Foo(@attr a: Int, @attr b: String, @text c: Double)
      @XmlCodec("bar")
      case class Bar(d: String, foo: Foo, e: Char)

      val bar    = Bar("d value", Foo(1, "b value", 3.0), 'e')
      val string = """<?xml version='1.0' encoding='UTF-8'?>
                     | <bar>
                     |   <d>d value</d>
                     |   <foo a="1" b="b value">3.0</foo>
                     |   <e>e</e>
                     | </bar>
                   """.stripMargin

      val decoded = XmlDecoder[Bar].decodeFromFoldable(toList(string))
      assert(decoded == Right(bar))
    }

    "decode text values sync" in decodeTextValues(pure)
    "decode text values async" in decodeTextValues(fromIterable)

    def decodeRecursiveValues(toList: String => List[Array[Byte]]): Assertion = {
      @XmlCodec("foo")
      case class Foo(foo: Option[Foo], das: Int)

      val foo    = Foo(Some(Foo(Some(Foo(Some(Foo(Some(Foo(None, 4)), 3)), 2)), 1)), 0)
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

      val decoded = XmlDecoder[Foo].decodeFromFoldable(toList(string))
      assert(decoded == Right(foo))
    }

    "decode recursive values sync" in decodeRecursiveValues(pure)
    "decode recursive values async" in decodeRecursiveValues(fromIterable)

    def decodeSealedTraits(toList: String => List[Array[Byte]]): Assertion = {
      @ElementCodec
      case class Bar(d: String, foo: SealedClasses.Foo, e: Char)

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

      assert(
        barDecoder.decodeFromFoldable(toList(string1)) == Right(bar1) &&
          barDecoder.decodeFromFoldable(toList(string2)) == Right(bar2) &&
          barDecoder.decodeFromFoldable(toList(string3)) == Right(bar3)
      )
    }

    "decode sealed traits sync" in decodeSealedTraits(pure)
    "decode sealed traits async" in decodeSealedTraits(fromIterable)

    def decodeSealedTraitsWithCustomDiscriminator(toList: String => List[Array[Byte]]): Assertion = {
      @ElementCodec
      case class Qux(d: String, bar: SealedClasses.Bar, e: Char)

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
        quxDecoder.decodeFromFoldable(toList(string1)) == Right(qux1) &&
          quxDecoder.decodeFromFoldable(toList(string2)) == Right(qux2) &&
          quxDecoder.decodeFromFoldable(toList(string3)) == Right(qux3)
      )

      @ElementCodec
      case class Quux(d: String, baz: SealedClasses.Baz, e: Char)

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
        quuxDecoder.decodeFromFoldable(toList(string4)) == Right(quux1) &&
          quuxDecoder.decodeFromFoldable(toList(string5)) == Right(quux2) &&
          quuxDecoder.decodeFromFoldable(toList(string6)) == Right(quux3)
      )
    }

    "decode sealed traits with custom discriminator sync" in decodeSealedTraitsWithCustomDiscriminator(pure)
    "decode sealed traits with custom discriminator async" in decodeSealedTraitsWithCustomDiscriminator(fromIterable)

    def ignoreExtraElements(toList: String => List[Array[Byte]]): Assertion = {
      @XmlCodec("foo")
      case class Foo(das: Int)

      val foo    = Foo(0)
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

      val decoded = XmlDecoder[Foo].decodeFromFoldable(toList(string))
      assert(decoded == Right(foo))

    }

    "ignore extra elements sync" in ignoreExtraElements(pure)
    "ignore extra elements async" in ignoreExtraElements(fromIterable)

    def decodeMixedContent(toList: String => List[Array[Byte]]): Assertion = {
      @XmlCodec("foo")
      case class Foo(count: Int, buz: String, @text text: String)

      val foo    = Foo(1, "Buzz", "Sending  item to ")
      val string = """<?xml version='1.0' encoding='UTF-8'?>
                     |<foo>Sending <count>1</count> item to <buz>Buzz</buz></foo>
                   """.stripMargin

      val decoded = XmlDecoder[Foo].decodeFromFoldable(toList(string))
      assert(decoded == Right(foo))

    }

    "decode mixed content sync" in decodeMixedContent(pure)
    "decode mixed content async" in decodeMixedContent(fromIterable)

    def escapeCharacters(toList: String => List[Array[Byte]]): Assertion = {
      @XmlCodec("foo")
      case class Foo(@attr baz: String, bar: String)

      val foo     = Foo("Esca\"'<>&pe", "Esca\"'<>&pe")
      val string  = """<?xml version='1.0' encoding='UTF-8'?>
                     |<foo baz="Esca&quot;&apos;&lt;>&amp;pe">
                     |  <bar>Esca"'&lt;>&amp;pe</bar>
                     |</foo>
                   """.stripMargin
      val decoded = XmlDecoder[Foo].decodeFromFoldable(toList(string))
      assert(decoded == Right(foo))
    }

    "escape characters sync" in escapeCharacters(pure)
    "escape characters async" in escapeCharacters(fromIterable)

    def decodeWithRenamed(toList: String => List[Array[Byte]]): Assertion = {
      @XmlCodec("foo")
      case class Foo(@attr baz: String, bar: String, @renamed("foooo") sample: String)

      val foo     = Foo("Esca\"'<>&pe", "Esca\"'<>&pe", "somefoo")
      val string  = """<?xml version='1.0' encoding='UTF-8'?>
                     |<foo baz="Esca&quot;&apos;&lt;>&amp;pe">
                     |  <bar>Esca"'&lt;>&amp;pe</bar>
                     |  <foooo>somefoo</foooo>
                     |</foo>
                   """.stripMargin
      val decoded = XmlDecoder[Foo].decodeFromFoldable(toList(string))
      assert(decoded == Right(foo))
    }

    "decodes with @renamed sync" in decodeWithRenamed(pure)
    "decodes with @renamed async" in decodeWithRenamed(fromIterable)

    def decodeWithRenamedNested(toList: String => List[Array[Byte]]): Assertion = {
      @XmlCodec("foo")
      case class Foo(@attr baz: String, @renamed("theBar") bar: Bar)

      @XmlCodec("bar")
      case class Bar(a: String, b: String)
      val foo     = Foo("Esca\"'<>&pe", Bar("theA", "theB"))
      val string  = """<?xml version='1.0' encoding='UTF-8'?>
                     |<foo baz="Esca&quot;&apos;&lt;>&amp;pe">
                     |  <theBar>
                     |    <a>theA</a>
                     |    <b>theB</b>
                     |  </theBar>
                     |</foo>
                   """.stripMargin
      val decoded = XmlDecoder[Foo].decodeFromFoldable(toList(string))
      assert(decoded == Right(foo))
    }

    "decodes with @renamed nested sync" in decodeWithRenamedNested(pure)
    "decodes with @renamed nested async" in decodeWithRenamedNested(fromIterable)

    def decodeWithRenamedAttribute(toList: String => List[Array[Byte]]): Assertion = {
      @XmlCodec("foo")
      case class Foo(@attr @renamed("theBaz") baz: String, bar: String)

      val foo     = Foo("Esca\"'<>&pe", "Esca\"'<>&pe")
      val string  = """<?xml version='1.0' encoding='UTF-8'?>
                     |<foo theBaz="Esca&quot;&apos;&lt;>&amp;pe">
                     |  <bar>Esca"'&lt;>&amp;pe</bar>
                     |</foo>
                   """.stripMargin
      val decoded = XmlDecoder[Foo].decodeFromFoldable(toList(string))
      assert(decoded == Right(foo))
    }

    "decodes with @renamed @attr sync" in decodeWithRenamedAttribute(pure)
    "decodes with @renamed @attr async" in decodeWithRenamedAttribute(fromIterable)

    def decodeRenamedTextValues(toList: String => List[Array[Byte]]): Assertion = {
      @ElementCodec
      case class Foo(@attr a: Int, @attr @renamed("theB") b: String, @text c: Double)
      @XmlCodec("bar")
      case class Bar(d: String, foo: Foo, e: Char)

      val bar    = Bar("d value", Foo(1, "b value", 3.0), 'e')
      val string = """<?xml version='1.0' encoding='UTF-8'?>
                     | <bar>
                     |   <d>d value</d>
                     |   <foo a="1" theB="b value">3.0</foo>
                     |   <e>e</e>
                     | </bar>
                   """.stripMargin

      val decoded = XmlDecoder[Bar].decodeFromFoldable(toList(string))
      assert(decoded == Right(bar))
    }

    "decode @renamed text values sync" in decodeRenamedTextValues(pure)
    "decode @renamed text values async" in decodeRenamedTextValues(fromIterable)

    def decodeCamelCase(toList: String => List[Array[Byte]]): Assertion = {
      val camelCaseConfig = ElementCodecConfig.default.withStyle(camelCase)
      @ElementCodec(camelCaseConfig)
      case class Foo(@attr someName: Int, @attr someOther: String, @text c: Double)
      @XmlCodec("Bar", camelCaseConfig)
      case class Bar(someTopName: String, someFoo: Foo, e: Char)

      val bar    = Bar("d value", Foo(1, "b value", 3.0), 'e')
      val string = """<?xml version='1.0' encoding='UTF-8'?>
                     | <Bar>
                     |   <SomeTopName>d value</SomeTopName>
                     |   <SomeFoo SomeName="1" SomeOther="b value">3.0</SomeFoo>
                     |   <E>e</E>
                     | </Bar>
                   """.stripMargin

      val decoded = XmlDecoder[Bar].decodeFromFoldable(toList(string))
      assert(decoded == Right(bar))
    }

    "decode CamelCase sync" in decodeCamelCase(pure)
    "decode CamelCase async" in decodeCamelCase(fromIterable)

    def decodeSnakeCase(toList: String => List[Array[Byte]]): Assertion = {
      val snakeCaseConfig = ElementCodecConfig.default.withStyle(snakeCase)
      @ElementCodec(snakeCaseConfig)
      case class Foo(@attr someName: Int, @attr someOther: String, @text c: Double)
      @XmlCodec("bar", snakeCaseConfig)
      case class Bar(someTopName: String, someFoo: Foo, e: Char)

      val bar    = Bar("d value", Foo(1, "b value", 3.0), 'e')
      val string = """<?xml version='1.0' encoding='UTF-8'?>
                     | <bar>
                     |   <some_top_name>d value</some_top_name>
                     |   <some_foo some_name="1" some_other="b value">3.0</some_foo>
                     |   <e>e</e>
                     | </bar>
                   """.stripMargin

      val decoded = XmlDecoder[Bar].decodeFromFoldable(toList(string))
      assert(decoded == Right(bar))
    }

    "decode snake_case sync" in decodeSnakeCase(pure)
    "decode snake_case async" in decodeSnakeCase(fromIterable)

    def decodeRenamedPriority(toList: String => List[Array[Byte]]): Assertion = {
      val snakeCaseConfig = ElementCodecConfig.default.withStyle(snakeCase)
      @ElementCodec(snakeCaseConfig)
      case class Foo(@attr someName: Int, @attr @renamed("i-Have-priority") someOther: String, @text c: Double)
      @XmlCodec("bar", snakeCaseConfig)
      case class Bar(someTopName: String, @renamed("Me2") someFoo: Foo, e: Char)

      val bar    = Bar("d value", Foo(1, "b value", 3.0), 'e')
      val string = """<?xml version='1.0' encoding='UTF-8'?>
                     | <bar>
                     |   <some_top_name>d value</some_top_name>
                     |   <Me2 some_name="1" i-Have-priority="b value">3.0</Me2>
                     |   <e>e</e>
                     | </bar>
                   """.stripMargin

      val decoded = XmlDecoder[Bar].decodeFromFoldable(toList(string))
      assert(decoded == Right(bar))
    }

    "decode with @renamed having priority over naming sync" in decodeRenamedPriority(pure)
    "decode with @renamed having priority over naming async" in decodeRenamedPriority(fromIterable)
  }

  "Decoder derivation with namespaces" should {

    def decodeSimpleCaseClasses(toList: String => List[Array[Byte]]): Assertion = {
      @XmlnsDef("tinkoff.ru")
      object tkf

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

      val decoded = XmlDecoder[Bar].decodeFromFoldable(toList(string))
      assert(decoded == Right(bar))
    }

    "decode simple case classes sync" in decodeSimpleCaseClasses(pure)
    "decode simple case classes async" in decodeSimpleCaseClasses(fromIterable)

    def decodeAttributes(toList: String => List[Array[Byte]]): Assertion = {
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
      val string = """<?xml version='1.0' encoding='UTF-8'?>
                     | <ans1:bar xmlns:ans1="tinkoff.ru" ans1:e="e">
                     |   <ans1:d>d value</ans1:d>
                     |   <ans1:foo ans1:b="b value">
                     |     <ans1:a>1</ans1:a>
                     |     <ans1:c>3.0</ans1:c>
                     |   </ans1:foo>
                     | </ans1:bar>
                   """.stripMargin

      val decoded = XmlDecoder[Bar].decodeFromFoldable(toList(string))
      assert(decoded == Right(bar))

    }

    "decode attributes sync" in decodeAttributes(pure)
    "decode attributes async" in decodeAttributes(fromIterable)

    def decodeNestedNamespaces(toList: String => List[Array[Byte]]): Assertion = {
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

      val bar     = Bar("d value", Foo(1, "b value", 3.0), 'e')
      val string  = """<?xml version='1.0' encoding='UTF-8'?>
                     | <bar e="e">
                     |   <d>d value</d>
                     |   <ans1:foo xmlns:ans1="tinkoff.ru" b="b value">
                     |     <ans1:a>1</ans1:a>
                     |     <ans1:c>3.0</ans1:c>
                     |   </ans1:foo>
                     | </bar>
                   """.stripMargin
      val decoded = XmlDecoder[Bar].decodeFromFoldable(toList(string))
      assert(decoded == Right(bar))

    }

    "decode nested namespace sync" in decodeNestedNamespaces(pure)
    "decode nested namespace async" in decodeNestedNamespaces(fromIterable)

    def decodeMultipleNamespaces(toList: String => List[Array[Byte]]): Assertion = {
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
      val string = """<?xml version='1.0' encoding='UTF-8'?>
                     | <ans1:bar xmlns:ans1="tcsbank.ru" e="e">
                     |   <ans1:d>d value</ans1:d>
                     |   <ans2:foo xmlns:ans2="tinkoff.ru" b="b value">
                     |     <ans2:a>1</ans2:a>
                     |     <ans2:c>3.0</ans2:c>
                     |   </ans2:foo>
                     | </ans1:bar>
                   """.stripMargin

      val decoded = XmlDecoder[Bar].decodeFromFoldable(toList(string))
      assert(decoded == Right(bar))

    }

    "decode multiple namespaces sync" in decodeMultipleNamespaces(pure)
    "decode multiple namespaces async" in decodeMultipleNamespaces(fromIterable)

    def decodeCamelCase(toList: String => List[Array[Byte]]): Assertion = {
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
          @xmlns(tkf) someFoo: Foo
      )

      val bar    = Bar("d value", Foo(1, "b value", 3.0))
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

      val decoded = XmlDecoder[Bar].decodeFromFoldable(toList(string))
      assert(decoded == Right(bar))
    }

    "decode CamelCase sync" in decodeCamelCase(pure)
    "decode CamelCase async" in decodeCamelCase(fromIterable)

    def decodeSnakeCase(toList: String => List[Array[Byte]]): Assertion = {
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
          @xmlns(tkf) someFoo: Foo
      )

      val bar    = Bar("d value", Foo(1, "b value", 3.0))
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

      val decoded = XmlDecoder[Bar].decodeFromFoldable(toList(string))
      assert(decoded == Right(bar))
    }

    "decode snake_case sync" in decodeSnakeCase(pure)
    "decode snake_case async" in decodeSnakeCase(fromIterable)
  }

  "Decoder derivation compilation" should {
    "fail if wrong attributes" in {
      """
        | @ElementCodec
        | case class NotAttribute(a: Int)
        | @ElementCodec
        | case class Wrapper(@attr attribute: NotAttribute)
      """.stripMargin shouldNot typeCheck
    }

    "fail if wrong texts" in {
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
