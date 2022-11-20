# phobos
[![Maven Central](https://img.shields.io/maven-central/v/ru.tinkoff/phobos-core_2.13.svg)](https://search.maven.org/search?q=ru.tinkoff.phobos-core)
[![Build](https://github.com/TinkoffCreditSystems/phobos/actions/workflows/scala.yml/badge.svg)](https://github.com/TinkoffCreditSystems/phobos/actions/workflows/scala.yml)
[![Scala Steward](https://img.shields.io/badge/Scala_Steward-helping-blue.svg?style=flat&logo=data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAA4AAAAQCAMAAAARSr4IAAAAVFBMVEUAAACHjojlOy5NWlrKzcYRKjGFjIbp293YycuLa3pYY2LSqql4f3pCUFTgSjNodYRmcXUsPD/NTTbjRS+2jomhgnzNc223cGvZS0HaSD0XLjbaSjElhIr+AAAAAXRSTlMAQObYZgAAAHlJREFUCNdNyosOwyAIhWHAQS1Vt7a77/3fcxxdmv0xwmckutAR1nkm4ggbyEcg/wWmlGLDAA3oL50xi6fk5ffZ3E2E3QfZDCcCN2YtbEWZt+Drc6u6rlqv7Uk0LdKqqr5rk2UCRXOk0vmQKGfc94nOJyQjouF9H/wCc9gECEYfONoAAAAASUVORK5CYII=)](https://scala-steward.org)
[![Discord](https://img.shields.io/badge/chat-discord%20(en%2Fru)-blue)](https://discord.gg/S9Ad88t)

Phobos is an XML data-binding library based on stream parsing. 
It depends only on [aalto-xml](https://github.com/FasterXML/aalto-xml/) for parsing.

Scala 2.12, 2.13 and 3.1 are supported. Scala 3.0 is supported in `core-3-0` module. See [Supported Scala versions](#supported-scala-versions) for more details.

## QuickStart
Add phobos-core to your dependencies:

```
libraryDependencies += "ru.tinkoff" %% "phobos-core" % "0.18.0"
```

Then try this code out in `sbt console` or in a separate source file:
```scala
import ru.tinkoff.phobos.decoding._
import ru.tinkoff.phobos.encoding._
import ru.tinkoff.phobos.syntax._
import ru.tinkoff.phobos.derivation.semiauto._

case class TravelPoint(country: String, city: String)
object TravelPoint {
  implicit val travelPointElementEncoder: ElementEncoder[TravelPoint] = deriveElementEncoder
  implicit val travelPointElementDecoder: ElementDecoder[TravelPoint] = deriveElementDecoder
}

case class Price(@attr currency: String, @text value: Double)
object Price {
  implicit val priceElementEncoder: ElementEncoder[Price] = deriveElementEncoder
  implicit val priceElementDecoder: ElementDecoder[Price] = deriveElementDecoder
}

case class Journey(price: Price, departure: TravelPoint, arrival: TravelPoint)
object Journey {
  implicit val journeyXmlEncoder: XmlEncoder[Journey] = deriveXmlEncoder("journey")
  implicit val journeyXmlDecoder: XmlDecoder[Journey] = deriveXmlDecoder("journey")
}


val journey =
  Journey(
    price = Price("EUR", 1000.0),
    departure = TravelPoint("France", "Marcelle"),
    arrival = TravelPoint("Germany", "Munich")
  )

val xml: String = XmlEncoder[Journey].encode(journey)
println(xml)

val decodedJourney = XmlDecoder[Journey].decode(xml)
println(decodedJourney)

assert(Right(journey) == decodedJourney)
```
Please see [phobos wiki](https://github.com/Tinkoff/phobos/wiki) for explanation of the syntax and more details.

## Performance
Performance details can be found out in [phobos-benchmark repository](https://github.com/valentiay/phobos-benchmark). 

## Modules
There are several additional modules for some specific cases. 
These modules could be added with command below:
```
libraryDependencies += "ru.tinkoff" %% "phobos-<module>" % "0.18.0"
```
Where `<module>` is module name.

| Module name | Functionality  |
|-------------|----------------|
| core-3-0    | Core module compiled for Scala 3.0. |
| ast         | Support for parsing XML into AST |
| akka-http   | Marshallers and unmarshallers for [akka-http](https://doc.akka.io/docs/akka-http/current/) |
| akka-stream | Streaming decoding support for [akka-stream](https://doc.akka.io/docs/akka/current/stream/index.html)|
| cats        | Cats instances |
| derevo      | Separate derivation of encoders and decoders separately using [derevo](https://github.com/manatki/derevo) annotations (e.g. `@derive(xmlEncoder("foo"))`)|
| enumeratum  | Support for [enumeratum](https://github.com/lloydmeta/enumeratum#manual-override-of-name) enums |
| fs2         | Streaming decoding support (`Stream[F, Array[Byte]] => G[A]`). Latest fs2 version (fs2 `3.x`, cats effect `3.x`) |
| fs2-ce2     | Streaming decoding support (`Stream[F, Array[Byte]] => G[A]`). Same as fs2 module, but for fs2 version `2.x` using cats effect `2.x`  |
| monix       | Streaming decoding support (`Observable[Array[Byte]] => Task[A]`)  |
| refined     | Support for [refined](https://github.com/fthomas/refined) |


## Supported Scala versions
Most modules support Scala 2.12, 2.13 and 3.1. 
Dependencies for some modules don't support Scala 3, thus these modules support only Scala 2.x.
Most module dependencies are compiled for Scala 3.1, and they can not be used with Scala 3.0, 
because Scala 3 [TASTy files are not forward compatible](https://contributors.scala-lang.org/t/improving-scala-3-forward-compatibility/5298).
Module `core` is also compiled for Scala 3.1, but core features may be used via `core-3-0` module.

Detailed information about supported Scala versions is in the table. Available versions for modules are marked with ✓.

| Module name | 2.12 | 2.13 | 3.0 | 3.1 |
|-------------|------|------|-----|-----|
| core        |  ✓   |  ✓   |     |  ✓  |
| core-3-0    |      |      |  ✓  |     |
| akka-http   |  ✓   |  ✓   |     |     |
| akka-stream |  ✓   |  ✓   |     |  ✓  |
| ast         |  ✓   |  ✓   |     |     |
| cats        |  ✓   |  ✓   |     |  ✓  |
| derevo      |  ✓   |  ✓   |     |     |
| enumeratum  |  ✓   |  ✓   |     |     |
| fs2         |  ✓   |  ✓   |     |  ✓  |
| fs2-ce2     |  ✓   |  ✓   |     |  ✓  |
| monix       |  ✓   |  ✓   |     |  ✓  |
| refined     |  ✓   |  ✓   |     |     |

## XSD and WSDL code-generation support
Classes from XSD could be generated using [deimos](https://github.com/Tinkoff/deimos) library.
