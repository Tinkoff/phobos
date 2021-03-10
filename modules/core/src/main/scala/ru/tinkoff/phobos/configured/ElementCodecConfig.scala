package ru.tinkoff.phobos.configured

import ru.tinkoff.phobos.Namespace

final case class ElementCodecConfig(
    transformAttributeNames: String => String,
    transformElementNames: String => String,
    transformConstructorNames: String => String,
    discriminatorLocalName: String,
    discriminatorNamespace: Option[String],
    useElementNameAsDiscriminator: Boolean,
    attributesDefaultNamespace: Option[String] = None,
    elementsDefaultNamespace: Option[String] = None,
    defineNamespaces: List[String] = Nil,
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

  def withAttributesDefaultNamespace(namespace: String): ElementCodecConfig =
    copy(attributesDefaultNamespace = Some(namespace))

  def withAttributesDefaultNamespace[NS](namespace: NS)(implicit ns: Namespace[NS]): ElementCodecConfig =
    copy(attributesDefaultNamespace = Some(ns.getNamespace))

  def withElementsDefaultNamespace(namespace: String): ElementCodecConfig =
    copy(elementsDefaultNamespace = Some(namespace))

  def withElementsDefaultNamespace[NS](namespace: NS)(implicit ns: Namespace[NS]): ElementCodecConfig =
    copy(elementsDefaultNamespace = Some(ns.getNamespace))

  def withNamespaceDefined(namespace: String): ElementCodecConfig =
    copy(defineNamespaces = namespace :: defineNamespaces)

  def withNamespaceDefined[NS](namespace: NS)(implicit ns: Namespace[NS]): ElementCodecConfig =
    copy(defineNamespaces = ns.getNamespace :: defineNamespaces)
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
      defineNamespaces = Nil,
    )
}
