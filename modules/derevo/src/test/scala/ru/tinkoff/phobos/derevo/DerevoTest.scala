package ru.tinkoff.phobos.derevo

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

class DerevoTest extends AnyWordSpec with Matchers {
  "Derevo" should {
    "derive xml encoder without namespace" in {
      """
        | import derevo.derive
        | import ru.tinkoff.phobos.derevo.xmlEncoder
        | import ru.tinkoff.phobos.encoding.XmlEncoder
        | @derive(xmlEncoder("foo"))
        | case class Foo(int: Int, string: String, double: Double)
        | implicitly[XmlEncoder[Foo]]
      """.stripMargin should compile
    }

    "derive xml encoder with namespace" in {
      """
        | import derevo.derive
        | import ru.tinkoff.phobos.annotations.XmlnsDef
        | import ru.tinkoff.phobos.derevo.xmlEncoder
        | import ru.tinkoff.phobos.encoding.XmlEncoder
        | @XmlnsDef("tinkoff.ru")
        | case object tkf
        | @derive(xmlEncoder("foo", tkf))
        | case class Foo(int: Int, string: String, double: Double)
        | implicitly[XmlEncoder[Foo]]
      """.stripMargin should compile
    }

    "derive xml decoder without namespace" in {
      """
        | import derevo.derive
        | import ru.tinkoff.phobos.derevo.xmlDecoder
        | import ru.tinkoff.phobos.decoding.XmlDecoder
        | @derive(xmlDecoder("foo"))
        | case class Foo(int: Int, string: String, double: Double)
        | implicitly[XmlDecoder[Foo]]
      """.stripMargin should compile
    }

    "derive xml decoder with namespace" in {
      """
        | import derevo.derive
        | import ru.tinkoff.phobos.annotations.XmlnsDef
        | import ru.tinkoff.phobos.derevo.xmlDecoder
        | import ru.tinkoff.phobos.decoding.XmlDecoder
        | @XmlnsDef("tinkoff.ru")
        | case object tkf
        | @derive(xmlDecoder("foo", tkf))
        | case class Foo(int: Int, string: String, double: Double)
        | implicitly[XmlDecoder[Foo]]
      """.stripMargin should compile
    }
  }
}
