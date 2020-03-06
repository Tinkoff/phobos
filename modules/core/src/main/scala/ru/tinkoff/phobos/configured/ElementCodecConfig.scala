package ru.tinkoff.phobos.configured

final case class ElementCodecConfig(
    transformAttributeNames: String => String,
    transformElementNames: String => String,
    discriminatorLocalName: String,
    discriminatorNamespace: Option[String]
) {
  def withElementsRenamed(transform: String => String): ElementCodecConfig =
    copy(transformElementNames = transform)

  def withAttributesRenamed(transform: String => String): ElementCodecConfig =
    copy(transformAttributeNames = transform)

  def withStyle(transform: String => String): ElementCodecConfig =
    copy(transformElementNames = transform, transformAttributeNames = transform)

  def withDiscriminator(localName: String, namespace: Option[String]): ElementCodecConfig =
    copy(discriminatorLocalName = localName, discriminatorNamespace = namespace)
}

object ElementCodecConfig {
  val default: ElementCodecConfig =
    ElementCodecConfig(identity, identity, "type", Some("http://www.w3.org/2001/XMLSchema-instance"))
}
