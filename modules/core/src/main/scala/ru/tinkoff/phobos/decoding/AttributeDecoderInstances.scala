package ru.tinkoff.phobos.decoding

trait AttributeDecoderInstances {

  implicit val stringDecoder: AttributeDecoder[String] =
    new AttributeDecoder[String] {
      def decodeAsAttribute(c: Cursor,
                            localName: String,
                            namespaceUri: Option[String]): Either[DecodingError, String] = {
        val idx = c.getAttributeIndex(namespaceUri.orNull, localName)
        if (idx > -1) {
          Right(c.getAttributeValue(idx))
        } else {
          Left(c.error(s"Missing '$localName' attribute"))
        }
      }
    }

  implicit def valueDecoder[A](implicit decoder: ValueDecoder[A]): AttributeDecoder[A] =
    stringDecoder.emap(decoder.decode)

  implicit def optionDecoder[A](implicit decoder: AttributeDecoder[A]): AttributeDecoder[Option[A]] =
    new AttributeDecoder[Option[A]] {
      def decodeAsAttribute(c: Cursor,
                            localName: String,
                            namespaceUri: Option[String]): Either[DecodingError, Option[A]] = {
        val idx = c.getAttributeIndex(namespaceUri.orNull, localName)
        if (idx > -1) {
          decoder.decodeAsAttribute(c, localName, namespaceUri).map(Some.apply)
        } else {
          Right(None)
        }
      }
    }
}
