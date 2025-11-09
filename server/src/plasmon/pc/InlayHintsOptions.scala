package plasmon.pc

// Based on https://github.com/scalameta/metals/blob/0ad0bc184f82dbd178d01f76913ea6bdfa98db14/metals/src/main/scala/scala/meta/internal/metals/InlayHintsOptions.scala#L3-L22

private case class InlayHintsOptions(options: Map[InlayHintsOption, Boolean]) {
  def inferredType: Boolean =
    options.getOrElse(InlayHintsOption.InferredType, false)
  def implicitConversions: Boolean =
    options.getOrElse(InlayHintsOption.ImplicitConversions, false)
  def implicitArguments: Boolean =
    options.getOrElse(InlayHintsOption.ImplicitArguments, false)
  def typeParameters: Boolean =
    options.getOrElse(InlayHintsOption.TypeParameters, false)
  def byNameParameters: Boolean =
    options.getOrElse(InlayHintsOption.ByNameParameters, false)
  def namedParameters: Boolean =
    options.getOrElse(InlayHintsOption.NamedParameters, false)
  def hintsInPatternMatch: Boolean =
    options.getOrElse(InlayHintsOption.HintsInPatternMatch, false)
  def closingLabels: Boolean =
    options.getOrElse(InlayHintsOption.ClosingLabels, false)
  def hintsXRayMode: Boolean =
    options.getOrElse(InlayHintsOption.HintsXRayMode, false)
}
