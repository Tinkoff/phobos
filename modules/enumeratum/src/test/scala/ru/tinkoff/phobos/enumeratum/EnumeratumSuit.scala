package ru.tinkoff.phobos.enumeratum

import java.util.concurrent.Executors

import enumeratum._
import monix.execution.Scheduler
import monix.reactive.Observable
import org.scalatest._
import ru.tinkoff.phobos.annotations.XmlCodec
import ru.tinkoff.phobos.decoding.XmlDecoder
import ru.tinkoff.phobos.encoding.XmlEncoder
import ru.tinkoff.phobos.syntax._
import ru.tinkoff.phobos.testString._

import scala.concurrent.Future

class EnumeratumSuit extends AsyncWordSpec with Matchers {
  implicit val scheduler: Scheduler = Scheduler(Executors.newScheduledThreadPool(4))
  "Enum codecs" should {
    "encode enums" in {
      sealed trait Foo extends EnumEntry with Product with Serializable
      object Foo extends XmlEnum[Foo] with Enum[Foo] {
        val values = findValues

        case object Foo1 extends Foo
        case object Foo2 extends Foo
        case object Foo3 extends Foo
      }
      import Foo._
      @XmlCodec("bar")
      case class Bar(d: String, foo: Foo, e: Char)
      @XmlCodec("baz")
      case class Baz(@attr f: Foo, @text text: Foo)
      val bar1 = Bar("d value", Foo1, 'e')
      val bar2 = Bar("d value", Foo2, 'e')
      val bar3 = Bar("another one value", Foo3, 'v')
      val baz  = Baz(Foo1, Foo2)
      val xml1 = XmlEncoder[Bar].encode(bar1)
      val xml2 = XmlEncoder[Bar].encode(bar2)
      val xml3 = XmlEncoder[Bar].encode(bar3)
      val xml4 = XmlEncoder[Baz].encode(baz)
      assert(
        xml1 ==
          """
              | <?xml version='1.0' encoding='UTF-8'?>
              | <bar>
              |   <d>d value</d>
              |   <foo>
              |     Foo1
              |   </foo>
              |   <e>e</e>
              | </bar>
            """.stripMargin.minimized &&
          xml2 ==
            """
                | <?xml version='1.0' encoding='UTF-8'?>
                | <bar>
                |   <d>d value</d>
                |   <foo>
                |     Foo2
                |   </foo>
                |   <e>e</e>
                | </bar>
              """.stripMargin.minimized &&
          xml3 ==
            """
                | <?xml version='1.0' encoding='UTF-8'?>
                | <bar>
                |   <d>another one value</d>
                |   <foo>
                |     Foo3
                |   </foo>
                |   <e>v</e>
                | </bar>
              """.stripMargin.minimized &&
          xml4 ==
            """
                | <?xml version='1.0' encoding='UTF-8'?>
                | <baz f="Foo1">Foo2</baz>
              """.stripMargin.minimized)
    }

    def pure(str: String): Observable[String] =
      Observable.pure(str)

    def fromIterable(str: String): Observable[String] =
      Observable.fromIterable(str.toIterable.map(_.toString))

    def decodeEnums(toObservable: String => Observable[String]): Future[Assertion] = {
      sealed trait Foo extends EnumEntry with Product with Serializable
      object Foo extends XmlEnum[Foo] with Enum[Foo] {
        val values = findValues

        case object Foo1 extends Foo

        case object Foo2 extends Foo

        case object Foo3 extends Foo

      }
      import Foo._
      @XmlCodec("bar")
      case class Bar(d: String, foo: Foo, e: Char)
      @XmlCodec("baz")
      case class Baz(@attr f: Foo, @text text: Foo)
      val bar1 = Bar("d value", Foo1, 'e')
      val bar2 = Bar("d value", Foo2, 'e')
      val bar3 = Bar("another one value", Foo3, 'v')
      val baz  = Baz(Foo1, Foo2)

      val string1 =
        """<?xml version='1.0' encoding='UTF-8'?>
          | <bar>
          |   <d>d value</d>
          |   <foo>Foo1</foo>
          |   <e>e</e>
          | </bar>
        """.stripMargin
      val string2 =
        """<?xml version='1.0' encoding='UTF-8'?>
          | <bar>
          |   <d>d value</d>
          |   <foo>Foo2</foo>
          |   <e>e</e>
          | </bar>
        """.stripMargin
      val string3 =
        """<?xml version='1.0' encoding='UTF-8'?>
          | <bar>
          |   <d>another one value</d>
          |   <foo>Foo3</foo>
          |   <e>v</e>
          | </bar>
        """.stripMargin
      val string4 =
        """<?xml version='1.0' encoding='UTF-8'?>
          | <baz f="Foo1">Foo2</baz>
        """.stripMargin
      (for {
        decoded1 <- XmlDecoder[Bar].decodeFromObservable(toObservable(string1))
        decoded2 <- XmlDecoder[Bar].decodeFromObservable(toObservable(string2))
        decoded3 <- XmlDecoder[Bar].decodeFromObservable(toObservable(string3))
        decoded4 <- XmlDecoder[Baz].decodeFromObservable(toObservable(string4))
      } yield {
        assert(decoded1 == bar1 && decoded2 == bar2 && decoded3 == bar3 && decoded4 == baz)
      }).runToFuture
    }

    "decode enums sync" in decodeEnums(pure)
    "decode enums async" in decodeEnums(fromIterable)
  }
}
