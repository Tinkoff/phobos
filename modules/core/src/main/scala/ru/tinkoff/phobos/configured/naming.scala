package ru.tinkoff.phobos.configured

object naming {
  val camelCase: String => String = _.capitalize

  val snakeCase: String => String = {
    _.replaceAll(
      "([A-Z]+)([A-Z][a-z])",
      "$1_$2"
    ).replaceAll("([a-z\\d])([A-Z])", "$1_$2")
      .toLowerCase
  }
}
