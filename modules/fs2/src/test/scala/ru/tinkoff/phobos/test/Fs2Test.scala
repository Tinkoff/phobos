package ru.tinkoff.phobos.test

import org.scalatest.AsyncWordSpec
import ru.tinkoff.phobos.annotations.{ElementCodec, XmlCodec}
import ru.tinkoff.phobos.decoding.XmlDecoder
import ru.tinkoff.phobos.syntax.text
import ru.tinkoff.phobos.fs2._
import fs2.Stream
import cats.effect.IO

class Fs2Test extends AsyncWordSpec{

  "Fs2 decoder" should {
    "decode case classes correctly" in {
      @ElementCodec
      case class Bar(@text txt: Int)
      @XmlCodec("foo")
      case class Foo(qux: Int, maybeBar: Option[Bar], bars: List[Bar])

      val xml = """
                  |<foo>
                  | <qux>1234</qux>
                  | <bars>2</bars>
                  | <maybeBar>1</maybeBar>
                  | <bars>3</bars>
                  |</foo>
                  |""".stripMargin

      val foo = Foo(1234, Some(Bar(1)), List(Bar(2), Bar(3)))
      val stream = Stream.fromIterator[IO](xml.toIterator.map(x => Array(x.toByte)))
      XmlDecoder[Foo]
        .decodeFromStream(stream)
        .map(result => assert(result == foo))
        .unsafeToFuture()
    }
  }
}
