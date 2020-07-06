package ru.tinkoff.phobos.configured

final case class ElementCodecConfig(
    transformAttributeNames: String => String,
    transformElementNames: String => String,
    transformConstructorNames: String => String,
    discriminatorLocalName: String,
    discriminatorNamespace: Option[String],
    useElementNameAsDiscriminator: Boolean,
) {
  def withElementsRenamed(transform: String => String): ElementCodecConfig =
    copy(transformElementNames = transform)

  def withAttributesRenamed(transform: String => String): ElementCodecConfig =
    copy(transformAttributeNames = transform)

  def withConstructorsRenamed(transform: String => String): ElementCodecConfig =
    copy(transformConstructorNames = transform)

  def withStyle(transform: String => String): ElementCodecConfig =
    copy(transformElementNames = transform, transformAttributeNames = transform)

  def withDiscriminator(localName: String, namespace: Option[String]): ElementCodecConfig =
    copy(discriminatorLocalName = localName, discriminatorNamespace = namespace)

  def usingElementNamesAsDiscriminator: ElementCodecConfig =
    copy(useElementNameAsDiscriminator = true)

}

object ElementCodecConfig {
  val default: ElementCodecConfig =
    ElementCodecConfig(
      transformAttributeNames = identity,
      transformElementNames = identity,
      transformConstructorNames = identity,
      discriminatorLocalName = "type",
      discriminatorNamespace = Some("http://www.w3.org/2001/XMLSchema-instance"),
      useElementNameAsDiscriminator = false,
    )
}
