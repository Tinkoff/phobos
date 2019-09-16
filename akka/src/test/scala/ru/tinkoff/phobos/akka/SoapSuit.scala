package ru.tinkoff.phobos.akka

import java.util.concurrent.Executors

import monix.execution.Scheduler
import org.scalatest._

class SoapSuit extends WordSpec with Matchers {
  implicit val scheduler: Scheduler = Scheduler(Executors.newScheduledThreadPool(4))
  "Soap codecs" should {
    "be found for envelope" in {
      """
       | import ru.tinkoff.phobos.encoding.XmlEncoder
       | import ru.tinkoff.phobos.decoding.XmlDecoder
       | import ru.tinkoff.phobos.annotations.ElementCodec
       | @ElementCodec
       | case class Header(foo: Int)
       | @ElementCodec
       | case class Body(bar: String)
       | implicitly[XmlEncoder[Envelope[Header, Body]]]
       | implicitly[XmlDecoder[Envelope[Header, Body]]]
      """.stripMargin should compile
    }

    "be found for headless envelope" in {
      """
       | import ru.tinkoff.phobos.encoding.XmlEncoder
       | import ru.tinkoff.phobos.decoding.XmlDecoder
       | import ru.tinkoff.phobos.annotations.ElementCodec
       | @ElementCodec
       | case class Body(bar: String)
       | implicitly[XmlEncoder[HeadlessEnvelope[Body]]]
       | implicitly[XmlDecoder[HeadlessEnvelope[Body]]]
      """.stripMargin should compile
    }
  }
}
