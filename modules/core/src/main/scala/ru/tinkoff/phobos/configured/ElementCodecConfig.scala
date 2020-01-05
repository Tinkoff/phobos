package ru.tinkoff.phobos.configured

final case class ElementCodecConfig(transformAttributeNames: String => String,
                                    transformElementNames: String => String) {
  def withElementsRenamed(transform: String => String): ElementCodecConfig   = copy(transformElementNames = transform)
  def withAttributesRenamed(transform: String => String): ElementCodecConfig = copy(transformAttributeNames = transform)
}
