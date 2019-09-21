
package ru.tinkoff.phobos.derevo

import org.manatki.derevo.{Derevo, Derivation, delegating}
import ru.tinkoff.phobos.encoding.XmlEncoder

@delegating("ru.tinkoff.phobos.derivation.semiauto.deriveXmlEncoder")
object xmlEncoder extends Derivation[XmlEncoder] {

  def apply[A](arg: String): XmlEncoder[A] = macro Derevo.delegateParam[XmlEncoder, A, String]

  def apply[A, NS](arg1: String, arg2: NS): XmlEncoder[A] = macro Derevo.delegateParams2[XmlEncoder, A, String, NS]
}
