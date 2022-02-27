package ru.tinkoff.phobos.refined.decoding

import eu.timepit.refined.api.{RefType, Validate}
import ru.tinkoff.phobos.decoding._
import scala.reflect.runtime.universe.TypeTag

trait DecodingInstances {

  implicit def refinedAttributeDecoder[F[_, _], T: TypeTag, P: TypeTag](
      implicit underlying: AttributeDecoder[T],
      refType: RefType[F],
      validate: Validate[T, P],
  ): AttributeDecoder[F[T, P]] =
    underlying.emap { (history, raw) =>
      refType.refine[P](raw) match {
        case Left(value)  => Left(mkDecodingError[T, P](raw, value, history))
        case Right(value) => Right(value)
      }
    }

  implicit def refinedTextDecoder[F[_, _], T: TypeTag, P: TypeTag](
      implicit underlying: TextDecoder[T],
      refType: RefType[F],
      validate: Validate[T, P],
  ): TextDecoder[F[T, P]] =
    underlying.emap { (history, raw) =>
      refType.refine[P](raw) match {
        case Left(value)  => Left(mkDecodingError[T, P](raw, value, history))
        case Right(value) => Right(value)
      }
    }

  implicit def refinedElementDecoder[F[_, _], T: TypeTag, P: TypeTag](
      implicit underlying: ElementDecoder[T],
      refType: RefType[F],
      validate: Validate[T, P],
  ): ElementDecoder[F[T, P]] =
    underlying.emap { (history, raw) =>
      refType.refine[P](raw) match {
        case Left(value)  => Left(mkDecodingError[T, P](raw, value, history))
        case Right(value) => Right(value)
      }
    }

  private def mkDecodingError[T: TypeTag, P: TypeTag](
      rawValue: T,
      error: String,
      history: List[String],
  ): DecodingError = {
    val T = implicitly[TypeTag[T]].tpe.toString
    val P = implicitly[TypeTag[P]].tpe.toString

    DecodingError(
      s"Failed to verify $P refinement for value=$rawValue of raw type $T: $error",
      history,
    )
  }
}
