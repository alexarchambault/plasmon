package plasmon.pc

// Based on https://github.com/scalameta/metals/blob/0ad0bc184f82dbd178d01f76913ea6bdfa98db14/metals/src/main/scala/scala/meta/internal/metals/InlayHintsOptions.scala#L38-L47

private sealed abstract class InlayHintsOption extends Product with Serializable

private object InlayHintsOption {
  case object InferredType        extends InlayHintsOption
  case object ImplicitConversions extends InlayHintsOption
  case object ImplicitArguments   extends InlayHintsOption
  case object TypeParameters      extends InlayHintsOption
  case object ByNameParameters    extends InlayHintsOption
  case object NamedParameters     extends InlayHintsOption
  case object HintsInPatternMatch extends InlayHintsOption
}
