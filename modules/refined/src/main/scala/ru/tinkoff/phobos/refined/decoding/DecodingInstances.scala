package ru.tinkoff.phobos.refined.decoding

import eu.timepit.refined.api.{RefType, Validate}
import ru.tinkoff.phobos.decoding._
import cats.syntax.either._
import scala.reflect.runtime.universe.TypeTag

trait DecodingInstances {
  implicit def refinedTextDecoder[F[_, _], T: TypeTag, P: TypeTag](
      implicit underlying: TextDecoder[T],
      refType: RefType[F],
      validate: Validate[T, P],
      decodingErrorVerbosity: DecodingErrorVerbosity[T, P] = DecodingErrorVerbosity.minVerbosity[T, P]
  ): TextDecoder[F[T, P]] = {
    underlying.emap { (history, raw) =>
      refType.refine[P](raw).leftMap(decodingErrorVerbosity.mkDecodingError(raw, _, history))
    }
  }

  implicit def refinedElementDecoder[F[_, _], T: TypeTag, P: TypeTag](
      implicit underlying: ElementDecoder[T],
      refType: RefType[F],
      validate: Validate[T, P],
      decodingErrorVerbosity: DecodingErrorVerbosity[T, P] = DecodingErrorVerbosity.minVerbosity[T, P]
  ): ElementDecoder[F[T, P]] = {
    underlying.emap { (history, raw) =>
      refType.refine[P](raw).leftMap(decodingErrorVerbosity.mkDecodingError(raw, _, history))
    }
  }
}
