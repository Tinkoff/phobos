package ru.tinkoff.phobos.configured

import ru.tinkoff.phobos.Namespace

/** Config, that can modify the behaviour of derived element decoders and encoders.
  *
  * This config usually affects only current case class or sealed trait - the one, for which the codec with config is
  * derived.
  *
  * Case class specific configuration.
  *
  * @param transformAttributeNames
  *   Function to transform names of attributes, that are defined in the current case class. No transformation is
  *   applied by default.
  * @param transformElementNames
  *   Function to transform names of elements, that are defined in the current case class. No transformation is applied
  *   by default.
  * @param attributesDefaultNamespace
  *   Namespace that will be used for all attributes, that are defined in the current case class and that don't have
  *   explicitly specified (via @xmlns annotation) namespace. Useful, when all or most of the attributes in the current
  *   case class have the same namespace. Default value is "None".
  * @param elementsDefaultNamespace
  *   Namespace that will be used for all elements, that are defined in the current case class and that don't have
  *   explicitly specified (via @xmlns annotation) namespace. Useful, when all or most of the elements in the current
  *   case class have the same namespace. Default value is "None".
  * @param scopeDefaultNamespace
  *   Default namespace for all elements in current scope. Current scope contains current elements and all children
  *   elements, recursively. Unlike most other settings, this setting affect not only current element, but all children
  *   elements as well. In derived encoders this namespace is declared as
  *   [[https://www.w3.org/TR/xml-names/#defaulting XML default namespace]] in starting tag of current element. Derived
  *   decoders expect this namespace either to be default or specified explicitly in elements, that don't have other
  *   namespace with higher priority. The [[scopeDefaultNamespace]] has lower priority than [[elementsDefaultNamespace]]
  *   and namespaces in @xmlns annotations. Scope default namespace can be overridden by [[scopeDefaultNamespace]] of a
  *   child element. Default value is "None".
  *
  * Sealed trait specific configuration.
  *
  * @param discriminatorLocalName
  *   Name of the attribute containing sealed trait discriminator. Default value is "type", as defined in
  *   https://www.w3.org/TR/xmlschema-1/#xsi_type
  * @param discriminatorNamespace
  *   Namespace of the attribute containing sealed trait discriminator. Default value is
  *   "http://www.w3.org/2001/XMLSchema-instance", as defined in https://www.w3.org/TR/xmlschema-1/#xsi_type
  * @param useElementNameAsDiscriminator
  *   Forces codecs to use element name as sealed trait discriminator, instead of special attribute. This setting
  *   overrides [[discriminatorLocalName]] and [[discriminatorNamespace]]. Default value is "false".
  * @param transformConstructorNames
  *   Function to transform constructor names, that are used as sealed trait discriminator. No transformation is applied
  *   by default.
  *
  * Common configuration.
  *
  * @param defineNamespaces
  *   Affects only encoders. Adds definition of the namespace in the starting tag of current element. First element of
  *   the tuple is URL, second - optional preferred prefix of the namespace. The namespaces are only declared, but are
  *   not assigned to any elements or attributes. These declarations may be used by nested elements. This setting helps
  *   to avoid duplicate namespace declarations. No namespaces are defined by default.
  */
final case class ElementCodecConfig(
    transformAttributeNames: String => String,
    transformElementNames: String => String,
    transformConstructorNames: String => String,
    discriminatorLocalName: String,
    discriminatorNamespace: Option[String],
    useElementNameAsDiscriminator: Boolean,
    attributesDefaultNamespace: Option[String] = None,
    elementsDefaultNamespace: Option[String] = None,
    defineNamespaces: List[(String, Option[String])] = Nil,
    scopeDefaultNamespace: Option[String] = None,
) {

  /** See docs for [[transformElementNames]]. */
  def withElementNamesTransformed(transform: String => String): ElementCodecConfig =
    copy(transformElementNames = transform)

  @deprecated("Use withElementNamesTransformed instead", "0.19.0")
  def withElementsRenamed(transform: String => String): ElementCodecConfig =
    withElementNamesTransformed(transform)

  /** See docs for [[transformAttributeNames]]. */
  def withAttributeNamesTransformed(transform: String => String): ElementCodecConfig =
    copy(transformAttributeNames = transform)

  @deprecated("Use withAttributeNamesTransformed instead", "0.19.0")
  def withAttributesRenamed(transform: String => String): ElementCodecConfig =
    withAttributeNamesTransformed(transform)

  /** See docs for [[transformConstructorNames]]. */
  def withConstructorNamesTransformed(transform: String => String): ElementCodecConfig =
    copy(transformConstructorNames = transform)

  @deprecated("Use withConstructorNamesTransformed instead", "0.19.0")
  def withConstructorsRenamed(transform: String => String): ElementCodecConfig =
    withConstructorNamesTransformed(transform)

  /** Transforms both element and attribute names. See docs for [[transformElementNames]] and
    * [[transformAttributeNames]].
    */
  def withFieldNamesTransformed(transform: String => String): ElementCodecConfig =
    copy(transformElementNames = transform, transformAttributeNames = transform)

  @deprecated("Use withFieldNamesTransformed instead", "0.19.0")
  def withStyle(transform: String => String): ElementCodecConfig =
    withFieldNamesTransformed(transform)

  /** Configures name and namespace of an attribute, that is used as sealed trait discriminator. See docs for
    * [[discriminatorLocalName]] and [[discriminatorNamespace]].
    */
  def withDiscriminator(localName: String, namespace: Option[String]): ElementCodecConfig =
    copy(discriminatorLocalName = localName, discriminatorNamespace = namespace)

  /** See docs for [[useElementNameAsDiscriminator]]. */
  def usingElementNamesAsDiscriminator: ElementCodecConfig =
    copy(useElementNameAsDiscriminator = true)

  /** See docs for [[attributesDefaultNamespace]]. */
  def withAttributesDefaultNamespace(namespace: String): ElementCodecConfig =
    copy(attributesDefaultNamespace = Some(namespace))

  /** See docs for [[attributesDefaultNamespace]]. */
  def withAttributesDefaultNamespace[NS](namespace: NS)(implicit ns: Namespace[NS]): ElementCodecConfig =
    copy(attributesDefaultNamespace = Some(ns.getNamespace))

  /** See docs for [[elementsDefaultNamespace]]. */
  def withElementsDefaultNamespace(namespace: String): ElementCodecConfig =
    copy(elementsDefaultNamespace = Some(namespace))

  /** See docs for [[elementsDefaultNamespace]]. */
  def withElementsDefaultNamespace[NS](namespace: NS)(implicit ns: Namespace[NS]): ElementCodecConfig =
    copy(elementsDefaultNamespace = Some(ns.getNamespace))

  /** See docs for [[defineNamespaces]]. */
  def withNamespaceDefined(namespace: String): ElementCodecConfig =
    copy(defineNamespaces = (namespace, None) :: defineNamespaces)

  /** See docs for [[defineNamespaces]]. */
  def withNamespaceDefined[NS](namespace: NS)(implicit ns: Namespace[NS]): ElementCodecConfig =
    copy(defineNamespaces = (ns.getNamespace, ns.getPreferredPrefix) :: defineNamespaces)

  /** See docs for [[scopeDefaultNamespace]]. */
  def withScopeDefaultNamespace(namespace: String): ElementCodecConfig =
    copy(scopeDefaultNamespace = Some(namespace))

  /** See docs for [[scopeDefaultNamespace]]. */
  def withScopeDefaultNamespace[NS](namespace: NS)(implicit ns: Namespace[NS]): ElementCodecConfig =
    copy(scopeDefaultNamespace = Some(ns.getNamespace))
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
