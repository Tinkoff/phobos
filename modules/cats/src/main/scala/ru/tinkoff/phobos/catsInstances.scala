package ru.tinkoff.phobos

import cats.{Contravariant, Foldable, Functor}
import cats.data.{Chain, NonEmptyChain, NonEmptyList, NonEmptySet, NonEmptyVector}
import javax.xml.stream.XMLStreamConstants
import ru.tinkoff.phobos.decoding.ElementDecoder.{listDecoder, vectorDecoder}
import ru.tinkoff.phobos.decoding.XmlDecoder.createStreamReader
import ru.tinkoff.phobos.decoding._
import ru.tinkoff.phobos.encoding.ElementEncoder.{iteratorEncoder, listEncoder, setEncoder, vectorEncoder}
import ru.tinkoff.phobos.encoding.{AttributeEncoder, ElementEncoder, TextEncoder}

object catsInstances {
  implicit val attributeEncoderContravariant: Contravariant[AttributeEncoder] =
    new Contravariant[AttributeEncoder] {
      def contramap[A, B](fa: AttributeEncoder[A])(f: B => A): AttributeEncoder[B] = fa.contramap(f)
    }

  implicit val elementEncoderContravariant: Contravariant[ElementEncoder] =
    new Contravariant[ElementEncoder] {
      def contramap[A, B](fa: ElementEncoder[A])(f: B => A): ElementEncoder[B] = fa.contramap(f)
    }

  implicit val textEncoderContravariant: Contravariant[TextEncoder] =
    new Contravariant[TextEncoder] {
      def contramap[A, B](fa: TextEncoder[A])(f: B => A): TextEncoder[B] = fa.contramap(f)
    }

  implicit val attributeDecoderFunctor: Functor[AttributeDecoder] =
    new Functor[AttributeDecoder] {
      def map[A, B](fa: AttributeDecoder[A])(f: A => B): AttributeDecoder[B] = fa.map(f)
    }

  implicit val elementDecoderFunctor: Functor[ElementDecoder] =
    new Functor[ElementDecoder] {
      def map[A, B](fa: ElementDecoder[A])(f: A => B): ElementDecoder[B] = fa.map(f)
    }

  implicit val textDecoderFunctor: Functor[TextDecoder] =
    new Functor[TextDecoder] {
      def map[A, B](fa: TextDecoder[A])(f: A => B): TextDecoder[B] = fa.map(f)
    }

  implicit def chainElementEncoder[A](implicit encoder: ElementEncoder[A]): ElementEncoder[Chain[A]] =
    iteratorEncoder[A].contramap(_.iterator)

  implicit def nonEmptyListElementEncoder[A](implicit encoder: ElementEncoder[A]): ElementEncoder[NonEmptyList[A]] =
    listEncoder[A].contramap(_.toList)

  implicit def nonEmptyVectorElementEncoder[A](implicit encoder: ElementEncoder[A]): ElementEncoder[NonEmptyVector[A]] =
    vectorEncoder[A].contramap(_.toVector)

  implicit def nonEmptySetElementEncoder[A](implicit encoder: ElementEncoder[A]): ElementEncoder[NonEmptySet[A]] =
    setEncoder[A].contramap(_.toSortedSet)

  implicit def nonEmptyChainElementEncoder[A](implicit encoder: ElementEncoder[A]): ElementEncoder[NonEmptyChain[A]] =
    chainElementEncoder[A].contramap(_.toChain)

  implicit def chainElementDecoder[A](implicit decoder: ElementDecoder[A]): ElementDecoder[Chain[A]] =
    listDecoder[A].map(Chain.fromSeq)

  implicit def nonEmptyListElementDecoder[A](implicit decoder: ElementDecoder[A]): ElementDecoder[NonEmptyList[A]] =
    listDecoder[A].emap((history, list) =>
      NonEmptyList
        .fromList(list)
        .fold[Either[DecodingError, NonEmptyList[A]]](Left(DecodingError("List is empty", history)))(Right.apply),
    )

  implicit def nonEmptyVectorElementDecoder[A](implicit decoder: ElementDecoder[A]): ElementDecoder[NonEmptyVector[A]] =
    vectorDecoder[A].emap((history, vector) =>
      NonEmptyVector
        .fromVector(vector)
        .fold[Either[DecodingError, NonEmptyVector[A]]](Left(DecodingError("Vector is empty", history)))(Right.apply),
    )

  implicit def nonEmptyChainElementDecoder[A](implicit decoder: ElementDecoder[A]): ElementDecoder[NonEmptyChain[A]] =
    chainElementDecoder[A].emap((history, chain) =>
      NonEmptyChain
        .fromChain(chain)
        .fold[Either[DecodingError, NonEmptyChain[A]]](Left(DecodingError("Chain is empty", history)))(Right.apply),
    )

  implicit class XmlDecoderCatsOps[A](val xmlDecoder: XmlDecoder[A]) extends AnyVal {
    def decodeFromFoldable[F[_]: Foldable](f: F[Array[Byte]], charset: String = "UTF-8"): Either[DecodingError, A] = {
      val sr: XmlStreamReader = createStreamReader(charset)
      val cursor              = new Cursor(sr)

      val a = Foldable[F].foldLeft(f, xmlDecoder.elementdecoder) { (decoder: ElementDecoder[A], bytes: Array[Byte]) =>
        sr.getInputFeeder.feedInput(bytes, 0, bytes.length)
        cursor.next()
        while (cursor.getEventType == XMLStreamConstants.DTD || cursor.getEventType == XMLStreamConstants.START_DOCUMENT) {
          cursor.next()
        }

        if (decoder.result(cursor.history).isRight) {
          decoder
        } else {
          decoder.decodeAsElement(cursor, xmlDecoder.localname, xmlDecoder.namespaceuri)
        }
      }
      sr.getInputFeeder.endOfInput()
      a.result(cursor.history)
    }
  }
}
