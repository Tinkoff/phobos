package ru.tinkoff.phobos

import org.scalatest.Assertion
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import ru.tinkoff.phobos.annotations.XmlCodec
import ru.tinkoff.phobos.decoding.{DecodingError, XmlDecoder}
import ru.tinkoff.phobos.syntax.{attr, text}

class LiteralDecodingSuit extends AnyWordSpec with Matchers {
  def pure(str: String): List[Array[Byte]] =
    List(str.getBytes("UTF-8"))

  def fromIterable(str: String): List[Array[Byte]] =
    str.toList.map(c => Array(c.toByte))

  "Literal decoders for attributes" should {
    def decodeAttributesWithCorrectValue(toList: String => List[Array[Byte]]): Assertion = {
      @XmlCodec("foo")
      final case class Foo(@attr status: "Ok")

      val foo = Foo("Ok")
      val string =
        """<?xml version='1.0' encoding='UTF-8'?>
          | <foo status="Ok"/>
        """.stripMargin
      val decoded = XmlDecoder[Foo].decodeFromFoldable(toList(string))
      assert(decoded == Right(foo))
    }

    "decode attributes with correct values sync" in decodeAttributesWithCorrectValue(pure)
    "decode attributes with correct values async" in decodeAttributesWithCorrectValue(fromIterable)

    def failOnAttributesWithIncorrectValue(toList: String => List[Array[Byte]]): Assertion = {
      @XmlCodec("foo")
      final case class Foo(@attr status: "Ok")

      val string =
        """<?xml version='1.0' encoding='UTF-8'?>
          | <foo status="Error"/>
        """.stripMargin
      val decoded = XmlDecoder[Foo].decodeFromFoldable(toList(string))
      decoded should
        matchPattern { case Left(DecodingError("Failed to decode literal type. Expected: Ok, actual: Error", _)) => }
    }

    "fail on attributes with incorrect value sync" in failOnAttributesWithIncorrectValue(pure)
    "fail on attributes with incorrect value async" in failOnAttributesWithIncorrectValue(fromIterable)
  }

  "Literal decoders for elements" should {
    def decodeElementsWithCorrectValue(toList: String => List[Array[Byte]]): Assertion = {
      @XmlCodec("foo")
      final case class Foo(status: "Ok")

      val foo = Foo("Ok")
      val string =
        """<?xml version='1.0' encoding='UTF-8'?>
          | <foo><status>Ok</status></foo>
        """.stripMargin
      val decoded = XmlDecoder[Foo].decodeFromFoldable(toList(string))
      assert(decoded == Right(foo))
    }

    "decode elements with correct values sync" in decodeElementsWithCorrectValue(pure)
    "decode elements with correct values async" in decodeElementsWithCorrectValue(fromIterable)

    def failOnElementsWithIncorrectValue(toList: String => List[Array[Byte]]): Assertion = {
      @XmlCodec("foo")
      final case class Foo(status: "Ok")

      val string =
        """<?xml version='1.0' encoding='UTF-8'?>
          | <foo><status>Error</status></foo>
        """.stripMargin
      val decoded = XmlDecoder[Foo].decodeFromFoldable(toList(string))
      decoded should
        matchPattern { case Left(DecodingError("Failed to decode literal type. Expected: Ok, actual: Error", _)) => }
    }

    "fail on elements with incorrect value sync" in failOnElementsWithIncorrectValue(pure)
    "fail on elements with incorrect value async" in failOnElementsWithIncorrectValue(fromIterable)
  }

  "Literal decoders for text" should {
    def decodeTextWithCorrectValue(toList: String => List[Array[Byte]]): Assertion = {
      @XmlCodec("foo")
      final case class Foo(@text status: "Ok")

      val foo = Foo("Ok")
      val string =
        """<?xml version='1.0' encoding='UTF-8'?>
          | <foo>Ok</foo>
        """.stripMargin
      val decoded = XmlDecoder[Foo].decodeFromFoldable(toList(string))
      assert(decoded == Right(foo))
    }

    "decode elements with correct values sync" in decodeTextWithCorrectValue(pure)
    "decode elements with correct values async" in decodeTextWithCorrectValue(fromIterable)

    def failOnElementsWithIncorrectValue(toList: String => List[Array[Byte]]): Assertion = {
      @XmlCodec("foo")
      final case class Foo(@text status: "Ok")

      val string =
        """<?xml version='1.0' encoding='UTF-8'?>
          | <foo>Error</foo>
        """.stripMargin
      val decoded = XmlDecoder[Foo].decodeFromFoldable(toList(string))
      decoded should
        matchPattern { case Left(DecodingError("Failed to decode literal type. Expected: Ok, actual: Error", _)) => }
    }

    "fail on elements with incorrect value sync" in failOnElementsWithIncorrectValue(pure)
    "fail on elements with incorrect value async" in failOnElementsWithIncorrectValue(fromIterable)
  }
}
