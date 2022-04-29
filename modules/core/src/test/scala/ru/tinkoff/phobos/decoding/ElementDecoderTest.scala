package ru.tinkoff.phobos.decoding

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import ru.tinkoff.phobos.derivation.semiauto.deriveXmlDecoder

import java.time.OffsetDateTime

class ElementDecoderTest extends AnyWordSpec with Matchers {
  "ElementDecoder instance" should {
    "exists for OffsetDateTime and works properly" in {

      case class Foo(date: OffsetDateTime)
      implicit val xmlDecoder: XmlDecoder[Foo] = deriveXmlDecoder[Foo]("Foo")

      XmlDecoder[Foo].decode("<Foo><date>2019-10-27T18:27:26.1279855+05:00</date></Foo>") match {
        case Left(failure) => fail(s"Decoding result expected, got: ${failure.getMessage}")
        case Right(value)  => value.date shouldBe OffsetDateTime.parse("2019-10-27T18:27:26.1279855+05:00");
      }
    }
  }
}
