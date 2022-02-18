package ru.tinkoff.phobos.encoding

import com.fasterxml.aalto.out.WriterConfig
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.io.ByteArrayOutputStream
import java.time.OffsetDateTime

class ElementEncoderTest extends AnyWordSpec with Matchers with EncoderTestBase {
  "ElementEncoder instance" should {
    "exists for OffsetDateTime and works properly" in {

      val buff = new ByteArrayOutputStream(512)
      val sw = buildStreamWriter(new WriterConfig(), buff)

      ElementEncoder[OffsetDateTime].encodeAsElement(
        OffsetDateTime.parse("2019-10-27T18:27:26.1279855+05:00"), sw, "Date", None
      )

      sw.flush()

      buff.toString shouldBe "<Date>2019-10-27T18:27:26.127985500+05:00</Date>"
    }
  }
}
