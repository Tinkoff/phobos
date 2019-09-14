# phobos
[![Maven Central](https://img.shields.io/maven-central/v/ru.tinkoff/phobos-core_2.12.svg)](https://search.maven.org/search?q=ru.tinkoff.phobos-core)

Phobos is an XML data-binding library based on stream parsing.

**Readme and wiki are under construction now.**

## QuickStart
Add phobos to your dependencies:

    libraryDependencies += "ru.tinkoff" %% "phobos-core" % "0.1.0"

Then paste this code in `sbt console`:

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
    
## More complex example
Please see [phobos wiki](https://github.com/TinkoffCreditSystems/phobos/wiki) for explanation of the syntax.

    import ru.tinkoff.phobos.decoding._
    import ru.tinkoff.phobos.syntax._
    import ru.tinkoff.phobos.derivation._
    // import ru.tinkoff.phobos.decoding._
    // import ru.tinkoff.phobos.syntax._
    // import ru.tinkoff.phobos.derivation._
    
    case class Price(@attr currency: String, @text value: Double)
    // defined class Price
    implicit val priceElementDecoder: ElementDecoder[Price] = deriveElementDecoder
    // priceElementDecoder: ru.tinkoff.phobos.decoding.ElementDecoder[Price] = ElementDecoder$macro$1$1@517d89cf
    
    case class TravelPoint(country: String, city: String)
    // defined class TravelPoint
    implicit val travelPointElementDecoder: ElementDecoder[TravelPoint] = deriveElementDecoder
    // travelPointElementDecoder: ru.tinkoff.phobos.decoding.ElementDecoder[TravelPoint] = ElementDecoder$macro$9$1@3813f9b6
    
    case class Journey(price: Price, departure: TravelPoint, arrival: TravelPoint)
    // defined class Journey
    implicit val journeyElementDecoder: ElementDecoder[Journey] = deriveElementDecoder
    // journeyElementDecoder: ru.tinkoff.phobos.decoding.ElementDecoder[Journey] = ElementDecoder$macro$19$1@582f1a2e
    implicit val journeyXmlDecoder: XmlDecoder[Journey] = XmlDecoder.fromElementDecoder("journey")
    // journeyXmlDecoder: ru.tinkoff.phobos.decoding.XmlDecoder[Journey] = ru.tinkoff.phobos.decoding.XmlDecoder$$anon$1@63030fc2
    
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
    
    // journeyXml: String =
    //     "
    //     <journey>
    //     ...
    
    println(XmlDecoder[Journey].decode(journeyXml))

## Performance
Performance details can be found out in [phobos-benchmark repository](https://github.com/valentiay/phobos-benchmark). 

## XSD and WSDL code-generation support
It will be implemented really soon in [deimos](https://github.com/TinkoffCreditSystems/deimos) library.