package ru.tinkoff.phobos.soap
import ru.tinkoff.phobos.Namespace

case class soapenv()

case object soapenv {
  implicit val soapenvNs: Namespace[soapenv.type] = Namespace.mkInstance("http://schemas.xmlsoap.org/soap/envelope/")
  implicit val soapenvNss: Namespace[soapenv] = Namespace.mkInstance("http://schemas.xmlsoap.org/soap/envelope/")
}