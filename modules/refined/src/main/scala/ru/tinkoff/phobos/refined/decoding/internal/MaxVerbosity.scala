package ru.tinkoff.phobos.refined.decoding.internal

import ru.tinkoff.phobos.decoding.DecodingError
import ru.tinkoff.phobos.refined.decoding.DecodingErrorVerbosity
import scala.reflect.runtime.universe.TypeTag

class MaxVerbosity[T, P](implicit Ttag: TypeTag[T], Ptag: TypeTag[P]) extends DecodingErrorVerbosity[T, P] {
  private val T = Ttag.tpe.toString
  private val P = Ptag.tpe.toString

  override def mkDecodingError(rawValue: T, error: String, history: List[String]): DecodingError = {
    DecodingError(
      s"Failed to verify $P refinement for value=$rawValue of raw type $T: $error",
      history
    )
  }
}
