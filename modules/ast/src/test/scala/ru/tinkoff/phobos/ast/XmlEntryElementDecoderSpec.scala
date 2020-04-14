package ru.tinkoff.phobos.ast

import ru.tinkoff.phobos.annotations.{ElementCodec, XmlCodec}

class XmlEntryElementDecoderSpec {
  @ElementCodec
  case class Foo(bar: Int, foo: String)
  @XmlCodec("root")
  case class Root(foo: Foo, x: Boolean)
}
