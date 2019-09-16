
package ru.tinkoff.phobos.derevo


import org.manatki.derevo.{Derevo, Derivation, delegating}
import ru.tinkoff.phobos.decoding.XmlDecoder

@delegating("ru.tinkoff.phobos.derivation.deriveXmlDecoder")
object xmlDecoder extends Derivation[XmlDecoder] {

  def apply[A](arg: String): XmlDecoder[A] = macro Derevo.delegateParam[XmlDecoder, A, String]

  def apply[A, NS](arg1: String, arg2: NS): XmlDecoder[A] = macro Derevo.delegateParams2[XmlDecoder, A, String, NS]
}
