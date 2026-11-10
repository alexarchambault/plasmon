package plasmon.integration

final case class Labelled[T](label: String, value: T) {

  /** Label of the recorded BSP data to replay for this value
    *
    * `-compat` variants only differ by the presentation compiler the server uses, not by the build,
    * so they replay the same recording as their non-`-compat` counterpart.
    */
  def recordingLabel: String =
    label.stripSuffix("-compat")
}
