package ru.tinkoff.phobos.decoding

case class DecodingError(text: String, history: List[String]) extends Exception {
  override def getMessage: String = {
    val trace = if (history.nonEmpty) {
      history.mkString("\tIn element '", "'\n\t\tin element '", "'")
    } else {
      "\tIn root element"
    }
    s"""Error while decoding XML: $text
       |$trace
     """.stripMargin
  }
}
