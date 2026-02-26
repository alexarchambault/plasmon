package plasmon.integration

import java.lang.Boolean as JBoolean

import scala.util.Properties

object TestParams {

  private def isCI = System.getenv("CI") != null

  def enableSilentOutput =
    Option(System.getenv("PLASMON_SILENT_OVERRIDE"))
      .map(_.toBoolean)
      .getOrElse(true)
  // Also disabled on CI for now, as that can be *very* verbose
  def printOutputOnError = !enableSilentOutput
  val enableOutputFrame =
    // On Windows, OutputFrame stuff crashes if we don't have an actual terminal
    (!Properties.isWin || io.github.alexarchambault.isterminal.IsTerminal.isTerminal()) &&
    !isCI

  def updateSnapshotsFast        = updateSnapshots
  def updateSnapshots            = isCI
  def updateAlternativeSnapshots = false

  def cleanUpAfterTests = true

}
