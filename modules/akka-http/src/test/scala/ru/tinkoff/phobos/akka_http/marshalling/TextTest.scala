package ru.tinkoff.phobos.akka_http.marshalling

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class TextTest extends AnyWordSpec with Matchers {
  "Text xml codecs" should {
    "be found for marshalling/unmarshalling" in {
      """
       | import akka.http.scaladsl.unmarshalling.FromRequestUnmarshaller
       | import akka.http.scaladsl.marshalling.ToResponseMarshaller
       | import ru.tinkoff.phobos.akka_http.marshalling.text._
       | import ru.tinkoff.phobos.annotations.XmlCodec
       | @XmlCodec("request")
       | case class Body(bar: String)
       | implicitly[FromRequestUnmarshaller[Body]]
       | implicitly[ToResponseMarshaller[Body]]
      """.stripMargin should compile
    }
  }
}
