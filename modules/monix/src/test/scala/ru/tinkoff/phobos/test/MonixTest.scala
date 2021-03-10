package ru.tinkoff.phobos.test

import java.util.concurrent.Executors

import monix.execution.Scheduler
import monix.reactive.Observable
import org.scalatest.wordspec.AsyncWordSpec
import ru.tinkoff.phobos.annotations.{ElementCodec, XmlCodec}
import ru.tinkoff.phobos.decoding.XmlDecoder
import ru.tinkoff.phobos.syntax.text
import ru.tinkoff.phobos.monix._

class MonixTest extends AsyncWordSpec {
  implicit val scheduler: Scheduler = Scheduler(Executors.newScheduledThreadPool(4))

  "Monix decoder" should {
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

      val foo        = Foo(1234, Some(Bar(1)), List(Bar(2), Bar(3)))
      val observable = Observable.fromIterable(xml.toIterable.map(x => Array(x.toByte)))
      XmlDecoder[Foo]
        .decodeFromObservable(observable)
        .map(result => assert(result == foo))
        .runToFuture
    }
  }
}
