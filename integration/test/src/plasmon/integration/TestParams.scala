package plasmon.integration

import scala.util.Properties

object TestParams {

  private def isCI = System.getenv("CI") != null

  def enableSilentOutput = true
  // Also disabled on CI for now, as that can be *very* verbose
  def printOutputOnError = !enableSilentOutput

  def updateSnapshotsFast        = updateSnapshots
  def updateSnapshots            = isCI
  def updateAlternativeSnapshots = false

  def cleanUpAfterTests = true

}
