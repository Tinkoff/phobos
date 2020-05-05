package ru.tinkoff.phobos.refined.decoding

import ru.tinkoff.phobos.decoding.DecodingError

trait DecodingErrorVerbosity[T, P] {
  def mkDecodingError(rawValue: T, error: String, history: List[String]): DecodingError
}

object DecodingErrorVerbosity {
  def apply[T, P](implicit ev: DecodingErrorVerbosity[T, P]): ev.type = ev

  def minVerbosity[T, P]: DecodingErrorVerbosity[T, P] =
    internal.MinVerbosity.asInstanceOf[DecodingErrorVerbosity[T, P]]
}
