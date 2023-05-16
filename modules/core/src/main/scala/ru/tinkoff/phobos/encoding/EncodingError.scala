package ru.tinkoff.phobos.encoding

case class EncodingError(text: String, cause: Option[Throwable] = None) extends Exception(text, cause.orNull) {
  override def getMessage: String = s"Error while encoding XML: $text"
}
