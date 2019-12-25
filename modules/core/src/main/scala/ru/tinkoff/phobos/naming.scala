package ru.tinkoff.phobos

sealed trait naming {
  def transformName(localName: String): String
}

object naming {
  case object asIs extends naming {
    override def transformName(localName: String): String = localName
  }
  case object camelCase extends naming {
    override def transformName(localName: String): String = localName.capitalize
  }
  case object snakeCase extends naming {
    override def transformName(localName: String): String =
      localName
        .replaceAll(
          "([A-Z]+)([A-Z][a-z])",
          "$1_$2"
        )
        .replaceAll("([a-z\\d])([A-Z])", "$1_$2")
        .toLowerCase
  }
}
