package ru.tinkoff.phobos.test

import akka.actor.ActorSystem
import akka.stream.scaladsl.{Sink, Source}
import akka.stream.{SystemMaterializer, Materializer}
import akka.testkit.TestKit
import org.scalatest.BeforeAndAfterAll
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.wordspec.AsyncWordSpecLike
import ru.tinkoff.phobos.akka_stream._
import ru.tinkoff.phobos.decoding.ElementDecoder
import ru.tinkoff.phobos.decoding.XmlDecoder
import ru.tinkoff.phobos.derivation.semiauto.deriveElementDecoder
import ru.tinkoff.phobos.derivation.semiauto.deriveXmlDecoder
import ru.tinkoff.phobos.syntax.text

import scala.concurrent.duration._

class AkkaStreamTest
    extends TestKit(ActorSystem("akka-stream-test")) with AsyncWordSpecLike with ScalaFutures with BeforeAndAfterAll {

  override implicit val patienceConfig: PatienceConfig = PatienceConfig(timeout = 5.seconds, interval = 200.millis)
  private implicit lazy val mat: Materializer          = SystemMaterializer(system).materializer

  override def beforeAll(): Unit = {
    system.registerOnTermination {
      mat.shutdown()
    }
  }

  override def afterAll(): Unit = system.terminate()

  import AkkaStreamTest._

  "Akka decodingFlow" should {
    "decode case classes correctly" in {

      val xml = """
                  |<foo>
                  | <qux>1234</qux>
                  | <bars>2</bars>
                  | <maybeBar>1</maybeBar>
                  | <bars>3</bars>
                  |</foo>
                  |""".stripMargin

      val foo         = Foo(1234, Some(Bar(1)), List(Bar(2), Bar(3)))
      val bytesSource = Source.fromIterator(() => xml.grouped(5).map(_.getBytes))

      val result = bytesSource
        .via(decodingFlow[Foo]())
        .runWith(Sink.head)
        .futureValue

      assert(result.isRight)
      assert(result contains foo)
    }

    "raise correct errors" in {

      val xml = """
                  |<foo>
                  | <quxar>safgd</quxar>
                  | <bars>2</bars>
                  | <maybeBar>1</maybeBar>
                  | <bars>3</bars>
                  |</foo>
                  |""".stripMargin

      val bytesSource = Source.fromIterator(() => xml.grouped(5).map(_.getBytes))

      val result = bytesSource
        .via(decodingFlow[Foo]())
        .runWith(Sink.head)
        .futureValue

      assert(result.isLeft)
    }

    "is reusable" in {

      val foo = Foo(1234, Some(Bar(1)), List(Bar(2), Bar(3)))
      val xml = """
                  |<foo>
                  | <qux>1234</qux>
                  | <bars>2</bars>
                  | <maybeBar>1</maybeBar>
                  | <bars>3</bars>
                  |</foo>
                  |""".stripMargin

      val fooFlow = decodingFlow[Foo]()

      def bytesSource = Source.fromIterator(() => xml.grouped(5).map(_.getBytes))

      val result = Source(1 to 20)
        .mapAsyncUnordered(parallelism = Runtime.getRuntime.availableProcessors()) { _ =>
          bytesSource
            .via(fooFlow)
            .runWith(Sink.head)
        }
        .runWith(Sink.seq)
        .futureValue

      assert(result.forall(_.contains(foo)))
    }
  }

  "Akka decodingFlowUnsafe" should {
    "decode case classes correctly" in {

      val xml = """
                  |<foo>
                  | <qux>1234</qux>
                  | <bars>2</bars>
                  | <maybeBar>1</maybeBar>
                  | <bars>3</bars>
                  |</foo>
                  |""".stripMargin

      val foo         = Foo(1234, Some(Bar(1)), List(Bar(2), Bar(3)))
      val bytesSource = Source.fromIterator(() => xml.grouped(5).map(_.getBytes))

      val result = bytesSource
        .via(decodingFlowUnsafe[Foo]())
        .runWith(Sink.head)
        .futureValue

      assert(result == foo)
    }
  }
}

object AkkaStreamTest {
  case class Bar(@text txt: Int)
  object Bar {
    implicit val barDecoder: ElementDecoder[Bar] = deriveElementDecoder
  }
  case class Foo(qux: Int, maybeBar: Option[Bar], bars: List[Bar])
  object Foo {
    implicit val foo: XmlDecoder[Foo] = deriveXmlDecoder("foo")
  }
}
