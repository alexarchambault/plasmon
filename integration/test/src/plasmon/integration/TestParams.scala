package plasmon.integration

import java.lang.{Boolean => JBoolean}

import scala.util.Properties

object TestParams {

  private def isCI = System.getenv("CI") != null

  def enableSilentOutput =
    Option(System.getenv("PLASMON_SILENT_OVERRIDE"))
      .map(_.toBoolean)
      .getOrElse(true)
  // Also disabled on CI for now, as that can be *very* verbose
  def printOutputOnError = !enableSilentOutput

  def updateSnapshotsFast        = updateSnapshots
  def updateSnapshots            = isCI
  def updateAlternativeSnapshots = false

  def cleanUpAfterTests = true

}
