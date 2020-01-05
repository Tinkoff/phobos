package ru.tinkoff.phobos.configured

object naming {
  val camelCase: ElementCodecConfig = {
    def capitalize(name: String): String = name.capitalize

    ElementCodecConfig(capitalize, capitalize)
  }

  val snakeCase: ElementCodecConfig = {
    def transform(name: String): String =
      name
        .replaceAll(
          "([A-Z]+)([A-Z][a-z])",
          "$1_$2"
        )
        .replaceAll("([a-z\\d])([A-Z])", "$1_$2")
        .toLowerCase

    ElementCodecConfig(transform, transform)
  }

  val asIs: ElementCodecConfig = ElementCodecConfig(identity, identity)
}
