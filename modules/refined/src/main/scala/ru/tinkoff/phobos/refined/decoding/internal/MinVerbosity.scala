package ru.tinkoff.phobos.refined.decoding.internal

import ru.tinkoff.phobos.decoding.DecodingError
import ru.tinkoff.phobos.refined.decoding.DecodingErrorVerbosity

object MinVerbosity extends DecodingErrorVerbosity[Any, Any] {
  override def mkDecodingError(rawValue: Any, error: String, history: List[String]): DecodingError = {
    DecodingError(error, history)
  }
}
