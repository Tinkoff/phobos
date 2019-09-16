package ru.tinkoff.phobos.akka

import ru.tinkoff.phobos.decoding.{ElementDecoder, XmlDecoder}
import ru.tinkoff.phobos.encoding.{ElementEncoder, XmlEncoder}
import ru.tinkoff.phobos.derivation
import ru.tinkoff.phobos.syntax.xmlns

final case class Envelope[Header, Body](@xmlns(soapenv) Header: Header, @xmlns(soapenv) Body: Body)

object Envelope {

  implicit def deriveEnvelopeEncoder[Header: ElementEncoder, Body: ElementEncoder]: XmlEncoder[Envelope[Header, Body]] = {
    implicit val envelopeElementEncoder: ElementEncoder[Envelope[Header, Body]] =
      derivation.deriveElementEncoder[Envelope[Header, Body]]

    XmlEncoder.fromElementEncoderNs[Envelope[Header, Body], soapenv]("Envelope")
  }

  implicit def deriveEnvelopeDecoder[Header: ElementDecoder, Body: ElementDecoder]: XmlDecoder[Envelope[Header, Body]] = {
    implicit val envelopeElementDecoder: ElementDecoder[Envelope[Header, Body]] =
      derivation.deriveElementDecoder[Envelope[Header, Body]]

    XmlDecoder.fromElementDecoderNs[Envelope[Header, Body], soapenv]("Envelope")
  }
}
