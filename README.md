# phobos
[![Maven Central](https://img.shields.io/maven-central/v/ru.tinkoff/phobos-core_2.13.svg)](https://search.maven.org/search?q=ru.tinkoff.phobos-core)
[![Discord](https://img.shields.io/badge/chat-discord%20(en%2Fru)-blue)](https://discord.gg/S9Ad88t)

Phobos is an XML data-binding library based on stream parsing. 
It depends on [cats-core](https://github.com/typelevel/cats) and 
[aalto-xml](https://github.com/FasterXML/aalto-xml/) for parsing.

Scala 2.12 and 2.13 are supported. Support for Scala 2.11 may be implemented on demand.

## QuickStart
Add phobos-core to your dependencies:

```
libraryDependencies += "ru.tinkoff" %% "phobos-core" % "0.3.1"
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
Please see [phobos wiki](https://github.com/TinkoffCreditSystems/phobos/wiki) for explanation of the syntax and more details.

## Performance
Performance details can be found out in [phobos-benchmark repository](https://github.com/valentiay/phobos-benchmark). 

## Addons
There are several additional modules for some specific cases. 
These modules could be added with command below:
```
libraryDependencies += "ru.tinkoff" %% "phobos-<module>" % "0.1.1"
```
Where `<module>` is module name.

| Module name  | Functionality  |
|--------------|----------------|
| akka         | Marshallers and unmarshallers for [akka-http](https://github.com/akka/akka-http) |
| derevo       | Separate derivation of encoders and decoders separately using [derevo](https://github.com/manatki/derevo) annotations (e.g. `@derive(xmlEncoder("foo"))`)
| enumeratum   | Support for [enumeratum](https://github.com/lloydmeta/enumeratum#manual-override-of-name) enums |
| fs2          | Streaming decoding support (`Stream[F, Array[Byte]] => G[A]`) |
| monix        | Streaming decoding support (`Observable[Array[Byte]] => Task[A]`)  |

## XSD and WSDL code-generation support
It will be soon implemented in [deimos](https://github.com/TinkoffCreditSystems/deimos) library.
