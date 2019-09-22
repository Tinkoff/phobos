package ru.tinkoff.phobos

import cats.syntax.option._
import org.scalatest._
import ru.tinkoff.phobos.annotations.{ElementCodec, XmlCodec, XmlCodecNs, XmlnsDef}
import ru.tinkoff.phobos.encoding.{AttributeEncoder, ElementEncoder, TextEncoder, XmlEncoder}
import ru.tinkoff.phobos.testString._
import ru.tinkoff.phobos.syntax._

class EncoderDerivationSuit extends WordSpec with Matchers {

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
          """.stripMargin.minimized)
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
          """.stripMargin.minimized)
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
      assert(xml ==
        """
          | <?xml version='1.0' encoding='UTF-8'?>
          | <qux>
          |   <str>constant</str>
          |   <foo bar="a74153b">text</foo>
          | </qux>
          """.stripMargin.minimized)
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
            """.stripMargin.minimized)
    }

    "encode lists" in {
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
          """.stripMargin.minimized)
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
          """.stripMargin.minimized)
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
          """.stripMargin.minimized)
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
          """.stripMargin.minimized)
    }

    //    "encode sealed traits" in {
    //      @ElementCodec
    //      sealed trait Foo
    //      object Foo {
    //        @ElementCodec
    //        case class Foo1(a: String) extends Foo
    //        @ElementCodec
    //        case class Foo2(b: Int) extends Foo
    //        @ElementCodec
    //        case class Foo3(c: Char) extends Foo
    //      }
    //      import Foo._
    //      @ElementCodec
    //      case class Bar(d: String, foo: Foo, e: Char)
    //
    //      val bar1       = Bar("d value", Foo1("string"), 'e')
    //      val bar2       = Bar("d value", Foo2(1), 'e')
    //      val bar3       = Bar("another one value", Foo3('c'), 'v')
    //      val barEncoder = XmlEncoder.fromTagEncoder[Bar]("bar")
    //      (for {
    //        xml1 <- barEncoder.encode(bar1).firstL
    //        xml2 <- barEncoder.encode(bar2).firstL
    //        xml3 <- barEncoder.encode(bar3).firstL
    //      } yield {
    //        assert(
    //          xml1 ==
    //            """
    //              | <?xml version='1.0' encoding='UTF-8'?>
    //              | <bar>
    //              |   <d>d value</d>
    //              |   <foo>
    //              |     <a>string</a>
    //              |   </foo>
    //              |   <e>e</e>
    //              | </bar>
    //            """.stripMargin.minimized &&
    //            xml2 ==
    //              """
    //                | <?xml version='1.0' encoding='UTF-8'?>
    //                | <bar>
    //                |   <d>d value</d>
    //                |   <foo>
    //                |     <b>1</b>
    //                |   </foo>
    //                |   <e>e</e>
    //                | </bar>
    //              """.stripMargin.minimized &&
    //            xml3 ==
    //              """
    //                | <?xml version='1.0' encoding='UTF-8'?>
    //                | <bar>
    //                |   <d>another one value</d>
    //                |   <foo>
    //                |     <c>c</c>
    //                |   </foo>
    //                |   <e>v</e>
    //                | </bar>
    //              """.stripMargin.minimized)
    //      }).runToFuture
    //    }

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
          """.stripMargin.minimized)
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
          """.stripMargin.minimized)
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
          """.stripMargin.minimized)
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
          """.stripMargin.minimized)
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
          """.stripMargin.minimized)
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
