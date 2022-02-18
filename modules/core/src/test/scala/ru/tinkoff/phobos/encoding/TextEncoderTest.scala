package ru.tinkoff.phobos.encoding

import com.fasterxml.aalto.out.WriterConfig
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.io.ByteArrayOutputStream
import java.time.OffsetDateTime

class TextEncoderTest extends AnyWordSpec with Matchers with EncoderTestBase {
  "TextEncoder instance" should {
    "exists for OffsetDateTime and works properly" in {

      val encoder = TextEncoder[OffsetDateTime]

      val buff = new ByteArrayOutputStream(512)
      val sw = buildStreamWriter(new WriterConfig(), buff)


      sw.writeStartElement("Foo")
      TextEncoder[OffsetDateTime].encodeAsText(
        OffsetDateTime.parse("2019-10-27T18:27:26.1279855+05:00"), sw
      )
      sw.writeEndElement()
      sw.flush()

      buff.toString shouldBe "<Foo>2019-10-27T18:27:26.127985500+05:00</Foo>"
    }
  }
}
