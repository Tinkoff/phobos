package ru.tinkoff.phobos.decoding

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import ru.tinkoff.phobos.annotations.XmlCodec
import ru.tinkoff.phobos.syntax.attr

import java.time.OffsetDateTime

class AttributeDecoderTest extends AnyWordSpec with Matchers {
  "AttributeDecoder instance" should {
    "exists for OffsetDateTime and works properly" in {

      @XmlCodec("Foo")
      case class Foo(@attr date: OffsetDateTime)

      XmlDecoder[Foo].decode("<Foo date=\"2019-10-27T18:27:26.1279855+05:00\"/>") match {
        case Left(failure) => fail(s"Decoding result expected, got: ${failure.getMessage}")
        case Right(value)  => value.date shouldBe OffsetDateTime.parse("2019-10-27T18:27:26.1279855+05:00");
      }
    }
  }
}
