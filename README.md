# phobos
[![Maven Central](https://img.shields.io/maven-central/v/ru.tinkoff/phobos-core_2.12.svg)](https://search.maven.org/search?q=ru.tinkoff.phobos-core)

Phobos is an XML data-binding library based on stream parsing.

**Readme and wiki are under construction now.**

## QuickStart
Add phobos to your dependencies:

    libraryDependencies += "ru.tinkoff" %% "phobos-core" % "0.1.0"

Then paste this code in `sbt console`:

```scala
import ru.tinkoff.phobos.decoding._
import ru.tinkoff.phobos.encoding._
import ru.tinkoff.phobos.syntax._
import ru.tinkoff.phobos.derivation._

case class Foo(@attr bar: Int, baz: String)
implicit val fooXmlEncoder: XmlEncoder[Foo] = deriveXmlEncoder("foo")
implicit val fooXmlDecoder: XmlDecoder[Foo] = deriveXmlDecoder("foo")

val foo = Foo(42, "fluffy")
val fooXml: String = XmlEncoder[Foo].encode(foo)
println(fooXml)
val decodedFoo = XmlDecoder[Foo].decode(fooXml)
assert(foo == decodedFoo)
```

## More complex example
Please see [phobos wiki](https://github.com/TinkoffCreditSystems/phobos/wiki) for explanation of the syntax.

```scala
import ru.tinkoff.phobos.decoding._
import ru.tinkoff.phobos.syntax._
import ru.tinkoff.phobos.derivation._

case class Price(@attr currency: String, @text value: Double)
implicit val priceElementDecoder: ElementDecoder[Price] = deriveElementDecoder

case class TravelPoint(country: String, city: String)
implicit val travelPointElementDecoder: ElementDecoder[TravelPoint] = deriveElementDecoder

case class Journey(price: Price, departure: TravelPoint, arrival: TravelPoint)
implicit val journeyElementDecoder: ElementDecoder[Journey] = deriveElementDecoder
implicit val journeyXmlDecoder: XmlDecoder[Journey] = XmlDecoder.fromElementDecoder("journey")

val journeyXml =
  """
    |<journey>
    |  <price currency="EUR">1000</price>
    |  <departure>
    |    <country>France</country>
    |    <city>Marcelle</city>
    |  </departure>
    |  <arrival>
    |    <country>Germany</country>
    |    <city>Munich</city>
    |  </arrival>
    |</journey>
    |""".stripMargin

println(XmlDecoder[Journey].decode(journeyXml))
// Journey(Price(EUR,1000.0),TravelPoint(France,Marcelle),TravelPoint(Germany,Munich))
```

## Performance
Performance details can be found out in [phobos-benchmark repository](https://github.com/valentiay/phobos-benchmark). 

## XSD and WSDL code-generation support
It will be implemented really soon in [deimos](https://github.com/TinkoffCreditSystems/deimos) library.
