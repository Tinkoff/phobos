package ru.tinkoff.phobos.akka

import ru.tinkoff.phobos.decoding.{ElementDecoder, XmlDecoder}
import ru.tinkoff.phobos.derivation
import ru.tinkoff.phobos.encoding.{ElementEncoder, XmlEncoder}
import ru.tinkoff.phobos.syntax.xmlns

final case class HeadlessEnvelope[Body](@xmlns(soapenv) Body: Body)

object HeadlessEnvelope {
  implicit def deriveEnvelopeEncoder[Body: ElementEncoder]: XmlEncoder[HeadlessEnvelope[Body]] = {
    implicit val envelopeElementEncoder: ElementEncoder[HeadlessEnvelope[Body]] =
      derivation.deriveElementEncoder[HeadlessEnvelope[Body]]

    XmlEncoder.fromElementEncoderNs[HeadlessEnvelope[Body], soapenv]("Envelope")
  }

  implicit def deriveEnvelopeDecoder[Body: ElementDecoder]: XmlDecoder[HeadlessEnvelope[Body]] = {
    implicit val envelopeElementDecoder: ElementDecoder[HeadlessEnvelope[Body]] =
      derivation.deriveElementDecoder[HeadlessEnvelope[Body]]

    XmlDecoder.fromElementDecoderNs[HeadlessEnvelope[Body], soapenv]("Envelope")
  }
}
