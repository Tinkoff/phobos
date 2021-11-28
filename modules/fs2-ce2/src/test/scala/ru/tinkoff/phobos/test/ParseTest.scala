package ru.tinkoff.phobos.test

import org.scalatest.wordspec.AsyncWordSpec
import ru.tinkoff.phobos.annotations.{ElementCodec, XmlCodec}
import ru.tinkoff.phobos.decoding.XmlDecoder
import ru.tinkoff.phobos.syntax.text
import ru.tinkoff.phobos.fs2._
import fs2.Stream
import cats.instances.future._
import cats.syntax.flatMap._
import cats.effect.IO
import org.scalatest.Inspectors
import ru.tinkoff.phobos.decoding.DecodingError

class ParseTest extends AsyncWordSpec with Inspectors {

  @XmlCodec("foo")
  case class Foo(@text txt: Int)

  object xml {
    val simpleSequential =
      ("root" :: Nil) ->
        """|<root>
         |  <foo>1</foo>
         |  <foo>2</foo>
         |  <foo>3</foo>
         |  <foo>4</foo>
         |</root>
         |""".stripMargin

    val nestedRepetetive =
      ("root" :: "sub" :: Nil) ->
        """|<root>
         |  <sub>
         |    <foo>1</foo>
         |    <foo>2</foo>
         |  </sub>
         |  <sub>
         |    <foo>3</foo>
         |    <foo>4</foo>
         |  </sub>
         |</root>
         |""".stripMargin

    val nestedRepetetiveIcnludingOtherTags =
      ("root" :: "sub" :: Nil) ->
        """|<root>
         |  <sub>
         |    <foo>1</foo>
         |    <!-- skip it -->
         |    <bar>nope</bar>
         |    <foo>2</foo>
         |  </sub>
         |  <sub>
         |    <foo>3</foo>
         |    <foo>4</foo>
         |  </sub>
         |  <!-- skip it too -->
         |  <bar>nope</bar>
         |  <sub>
         |    <foo>5</foo>
         |  </sub>
         |  <sub>
         |    <!-- and this one -->
         |    <bar>nope</bar>
         |  </sub>
         |</root>
         |""".stripMargin

    val all = simpleSequential :: nestedRepetetive :: nestedRepetetiveIcnludingOtherTags :: Nil
  }

  val expectations = Map(
    xml.simpleSequential -> Vector(1, 2, 3, 4).map(Foo(_)).map(Right(_)),
    xml.nestedRepetetive -> Vector(1, 2, 3, 4).map(Foo(_)).map(Right(_)),
    xml.nestedRepetetiveIcnludingOtherTags -> Vector(
      Right(Foo(1)),
      Left(DecodingError("Invalid local name. Expected 'foo', but found 'bar'", List("bar", "sub", "root"))),
      Right(Foo(2)),
      Right(Foo(3)),
      Right(Foo(4)),
      Right(Foo(5)),
      Left(DecodingError("Invalid local name. Expected 'foo', but found 'bar'", List("bar", "sub", "root"))),
    ),
  )

  def readAtOnce(path: List[String], xmlString: String) = {
    val streamBuilder = path.tail.foldLeft(Parse.oneDocument(path.head))(_.inElement(_))
    Stream.emits[IO, Byte](xmlString.getBytes).through(streamBuilder.everyElementAs[Foo].toFs2Stream[IO])
  }

  def readByteByByte(path: List[String], xmlString: String) = {
    val streamBuilder = path.tail.foldLeft(Parse.oneDocument(path.head))(_.inElement(_))
    Stream.emits[IO, Byte](xmlString.getBytes).unchunk.through(streamBuilder.everyElementAs[Foo].toFs2Stream[IO])
  }

  def assertStreamResult(stream: Stream[IO, Either[DecodingError, Foo]])(expects: Vector[Either[DecodingError, Foo]]) =
    stream.compile.toVector.map(result => assert(result === expects)).unsafeToFuture()

  "Parse" should {
    "handle all the elements" when {
      "oneDocument called with simpleSequential xml" in {
        val testCase @ (path, xmlString) = xml.simpleSequential

        for {
          r1 <- assertStreamResult(readAtOnce(path, xmlString))(expectations(testCase))
          r2 <- assertStreamResult(readByteByByte(path, xmlString))(expectations(testCase))
        } yield succeed
      }

      "oneDocument called with nestedRepetetive xml" in {
        val testCase @ (path, xmlString) = xml.nestedRepetetive

        for {
          r1 <- assertStreamResult(readAtOnce(path, xmlString))(expectations(testCase))
          r2 <- assertStreamResult(readByteByByte(path, xmlString))(expectations(testCase))
        } yield succeed
      }

      "oneDocument called with nestedRepetetiveIcnludingOtherTags xml" in {
        val testCase @ (path, xmlString) = xml.nestedRepetetiveIcnludingOtherTags

        for {
          r1 <- assertStreamResult(readAtOnce(path, xmlString))(expectations(testCase))
          r2 <- assertStreamResult(readByteByByte(path, xmlString))(expectations(testCase))
        } yield succeed
      }
    }

  }
}
