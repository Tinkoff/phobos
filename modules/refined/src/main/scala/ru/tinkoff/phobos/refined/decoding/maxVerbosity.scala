package ru.tinkoff.phobos.refined.decoding

import scala.reflect.runtime.universe.TypeTag

object maxVerbosity {
  implicit def decodingErrorMaxVerbosityImpl[T: TypeTag, P: TypeTag]: DecodingErrorVerbosity[T, P] =
    new internal.MaxVerbosity[T, P]
}
