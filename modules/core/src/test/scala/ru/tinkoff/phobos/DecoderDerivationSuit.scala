package ru.tinkoff.phobos

import cats.syntax.option._
import cats.instances.stream._
import org.scalatest._
import ru.tinkoff.phobos.annotations.{ElementCodec, XmlCodec, XmlCodecNs, XmlnsDef}
import ru.tinkoff.phobos.decoding.{AttributeDecoder, ElementDecoder, TextDecoder, XmlDecoder}
import ru.tinkoff.phobos.syntax._

class DecoderDerivationSuit extends WordSpec with Matchers {
  def pure(str: String): Stream[Array[Byte]] =
    Stream(str.getBytes("UTF-8"))

  def fromIterable(str: String): Stream[Array[Byte]] =
    str.toStream.map(c => Array(c.toByte))

  "Decoder derivation without namespaces" should {

    def decodeSimpleCaseClasses(toStream: String => Stream[Array[Byte]]): Assertion = {
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

      val decoded = XmlDecoder[Bar].decodeFromFoldable(toStream(string))
      assert(decoded == Right(bar))
    }

    "decode simple case classes sync" in decodeSimpleCaseClasses(pure)
    "decode simple case classes async" in decodeSimpleCaseClasses(fromIterable)

    def decodeAttributes(toStream: String => Stream[Array[Byte]]): Assertion = {
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
      val decoded = XmlDecoder[Bar].decodeFromFoldable(toStream(string))
      assert(decoded == Right(bar))
    }

    "decode attributes sync" in decodeAttributes(pure)
    "decode attributes async" in decodeAttributes(fromIterable)

    def allowToOverrideCodecs(toStream: String => Stream[Array[Byte]]): Assertion = {
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
      val decoded = XmlDecoder[Qux].decodeFromFoldable(toStream(string))
      assert(decoded == Right(qux))

    }

    "allow to override codecs sync" in allowToOverrideCodecs(pure)
    "allow to override codecs async" in allowToOverrideCodecs(fromIterable)

    def decodeOptions(toStream: String => Stream[Array[Byte]]): Assertion = {
      @ElementCodec
      case class Foo(a: Int, @attr b: String, c: Double)
      @XmlCodec("Wrapper")
      case class Wrapper(foo: Option[Foo])

      val opt1 = Wrapper(Some(Foo(1, "b", 2.0)))
      val opt2 = Wrapper(None)

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
      val decoded1 = XmlDecoder[Wrapper].decodeFromFoldable(toStream(string1))
      val decoded2 = XmlDecoder[Wrapper].decodeFromFoldable(toStream(string2))
      assert(decoded1 == Right(opt1) && decoded2 == Right(opt2))
    }

    "decode options sync" in decodeOptions(pure)
    "decode options async" in decodeOptions(fromIterable)

    def decodeNilValues(toStream: String => Stream[Array[Byte]]): Assertion = {
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

      val decoded1 = XmlDecoder[Wrapper1].decodeFromFoldable(toStream(string1))
      val decoded2 = XmlDecoder[Wrapper2].decodeFromFoldable(toStream(string2))
      assert(decoded1 == Right(wrapper1) && decoded2 == Right(wrapper2))
    }

    "decode nil values sync" in decodeNilValues(pure)
    "decode nil values async" in decodeNilValues(fromIterable)

    def decodeLists(toStream: String => Stream[Array[Byte]]): Assertion = {
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

      val decoded1 = XmlDecoder[Foos].decodeFromFoldable(toStream(string1))
      val decoded2 = XmlDecoder[Foos].decodeFromFoldable(toStream(string2))
      assert(decoded1 == Right(bar1) && decoded2 == Right(bar2))
    }

    "decode lists sync" in decodeLists(pure)
    "decode lists async" in decodeLists(fromIterable)

    def decodeMixedLists(toStream: String => Stream[Array[Byte]]): Assertion = {
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

      val decoded1 = XmlDecoder[FooBars].decodeFromFoldable(toStream(string1))
      val decoded2 = XmlDecoder[FooBars].decodeFromFoldable(toStream(string2))
      assert(decoded1 == Right(bar1) && decoded2 == Right(bar2))
    }

    "decode mixed lists sync" in decodeMixedLists(pure)
    "decode mixed lists async" in decodeMixedLists(fromIterable)

    def decodeByteArrays(toStream: String => Stream[Array[Byte]]): Assertion = {
      @XmlCodec("foo")
      case class Foo(@text content: Array[Byte])
      val foo = Foo("foobar".getBytes)

      val string = """<?xml version='1.0' encoding='UTF-8'?>
                     | <foo>Zm9vYmFy</foo>
                    """.stripMargin

      val decoded = XmlDecoder[Foo].decodeFromFoldable(toStream(string))
      assert(decoded.map(_.content.deep) == Right(foo.content.deep))
    }

    "decode byte arrays sync" in decodeByteArrays(pure)
    "decode byte arrays async" in decodeByteArrays(fromIterable)

    def decodeTextValues(toStream: String => Stream[Array[Byte]]): Assertion = {
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

      val decoded = XmlDecoder[Bar].decodeFromFoldable(toStream(string))
      assert(decoded == Right(bar))
    }

    "decode text values sync" in decodeTextValues(pure)
    "decode text values async" in decodeTextValues(fromIterable)

    def decodeRecursiveValues(toStream: String => Stream[Array[Byte]]): Assertion = {
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

      val decoded = XmlDecoder[Foo].decodeFromFoldable(toStream(string))
      assert(decoded == Right(foo))
    }

    "decode recursive values sync" in decodeRecursiveValues(pure)
    "decode recursive values async" in decodeRecursiveValues(fromIterable)

    def ignoreExtraElements(toStream: String => Stream[Array[Byte]]): Assertion = {
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

      val decoded = XmlDecoder[Foo].decodeFromFoldable(toStream(string))
      assert(decoded == Right(foo))

    }

    "ignore extra elements sync" in ignoreExtraElements(pure)
    "ignore extra elements async" in ignoreExtraElements(fromIterable)

    def decodeMixedContent(toStream: String => Stream[Array[Byte]]): Assertion = {
      @XmlCodec("foo")
      case class Foo(count: Int, buz: String, @text text: String)

      val foo    = Foo(1, "Buzz", "Sending  item to ")
      val string = """<?xml version='1.0' encoding='UTF-8'?>
                     |<foo>Sending <count>1</count> item to <buz>Buzz</buz></foo>
                   """.stripMargin

      val decoded = XmlDecoder[Foo].decodeFromFoldable(toStream(string))
      assert(decoded == Right(foo))

    }

    "decode mixed content sync" in decodeMixedContent(pure)
    "decode mixed content async" in decodeMixedContent(fromIterable)
  }

  "Decoder derivation with namespaces" should {

    def decodeSimpleCaseClasses(toStream: String => Stream[Array[Byte]]): Assertion = {
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

      val decoded = XmlDecoder[Bar].decodeFromFoldable(toStream(string))
      assert(decoded == Right(bar))
    }

    "decode simple case classes sync" in decodeSimpleCaseClasses(pure)
    "decode simple case classes async" in decodeSimpleCaseClasses(fromIterable)

    def decodeAttributes(toStream: String => Stream[Array[Byte]]): Assertion = {
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

      val decoded = XmlDecoder[Bar].decodeFromFoldable(toStream(string))
      assert(decoded == Right(bar))

    }

    "decode attributes sync" in decodeAttributes(pure)
    "decode attributes async" in decodeAttributes(fromIterable)

    def decodeNestedNamespaces(toStream: String => Stream[Array[Byte]]): Assertion = {
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
      val decoded = XmlDecoder[Bar].decodeFromFoldable(toStream(string))
      assert(decoded == Right(bar))

    }

    "decode nested namespace sync" in decodeNestedNamespaces(pure)
    "decode nested namespace async" in decodeNestedNamespaces(fromIterable)

    def decodeMultipleNamespaces(toStream: String => Stream[Array[Byte]]): Assertion = {
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

      val decoded = XmlDecoder[Bar].decodeFromFoldable(toStream(string))
      assert(decoded == Right(bar))

    }

    "decode multiple namespaces sync" in decodeMultipleNamespaces(pure)
    "decode multiple namespaces async" in decodeMultipleNamespaces(fromIterable)
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
