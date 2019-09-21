package ru.tinkoff.phobos

import java.util.concurrent.Executors

import monix.execution.Scheduler
import monix.reactive.Observable
import org.scalatest.{Assertion, AsyncWordSpec, Matchers}
import ru.tinkoff.phobos.annotations.{XmlCodec, XmlCodecNs, XmlnsDef}
import ru.tinkoff.phobos.decoding.XmlDecoder
import ru.tinkoff.phobos.syntax.{attr, text}

import scala.concurrent.Future

class DecoderDerivationErrorsSuit extends AsyncWordSpec with Matchers {
  implicit val scheduler: Scheduler = Scheduler(Executors.newScheduledThreadPool(4))

  def pure(str: String): Observable[String] =
    Observable.pure(str)

  def fromIterable(str: String): Observable[String] =
    Observable.fromIterable(str.toIterable.map(_.toString))

  "Text decoder" should {

    def failOnWrongNamespaces(toObservable: String => Observable[String]): Future[Assertion] = {
      @XmlCodecNs("foo", tkf)
      case class Foo1(@attr a: Int, @text c: Double)
      @XmlCodec("foo")
      case class Foo2(@attr a: Int, @text c: Double)

      @XmlnsDef("tinkoff.ru")
      case object tkf

      val string =
        """<?xml version='1.0' encoding='UTF-8'?>
            | <foo a="1" xmlns="http://wrong.com">
            |     1.1
            | </foo>
          """.stripMargin

      (for {
        foo <- XmlDecoder[Foo1].decodeFromObservable(toObservable(string)).attempt
        bar <- XmlDecoder[Foo2].decodeFromObservable(toObservable(string)).attempt
      } yield assert(foo.isLeft && bar.isLeft)).runToFuture
    }

    "fail on wrong namespaces sync" in failOnWrongNamespaces(pure)
    "fail on wrong namespaces async" in failOnWrongNamespaces(fromIterable)
  }
}
