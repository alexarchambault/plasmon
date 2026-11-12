package plasmon.integration

import scala.util.Properties

object TestParams {

  private def isCI = System.getenv("CI") != null

  def enableSilentOutput =
    Option(System.getenv("PLASMON_SILENT_OVERRIDE"))
      .map(_.toBoolean)
      .getOrElse(true)
  // Server output is captured to per-test log files (see TestLogs). Dumping it
  // here would interleave huge logs when tests run in parallel.
  def printOutputOnError = !enableSilentOutput
  val enableOutputFrame =
    // On Windows, OutputFrame stuff crashes if we don't have an actual terminal
    (!Properties.isWin || io.github.alexarchambault.isterminal.IsTerminal.isTerminal()) &&
    !isCI

  def updateSnapshotsFast = updateSnapshots
  def updateSnapshots     = isCI

  /** Whether to run the real build tools and refresh the recorded BSP data from them.
    *
    * Deliberately *not* tied to [[updateSnapshots]]: the point of recording is that CI doesn't have
    * to pay for a Mill / sbt / Scala CLI import on every run, so this is opt-in only. Set
    * `PLASMON_RECORD_BSP_DATA=true` after changing a test project, a build tool version, or
    * anything else that shifts what the build tools report.
    */
  def recordBspData =
    Option(System.getenv("PLASMON_RECORD_BSP_DATA")).exists(_.toBoolean)

  def cleanUpAfterTests = true

  /** Whether to start servers with `--sourcepath`, so that presentation compilers read the
    * workspace's modules from their sources rather than from built output.
    *
    * Set `PLASMON_SOURCEPATH=true`. Build tools are then only asked to set up the project (BSP
    * details, dependencies), never to compile it, and no `compile` command is sent to the server.
    * `PLASMON_SOURCEPATH_MODE` picks the `--sourcepath-mode`, when set.
    */
  def sourcepath =
    Option(System.getenv("PLASMON_SOURCEPATH")).exists(_.toBoolean)
  def sourcepathMode =
    Option(System.getenv("PLASMON_SOURCEPATH_MODE")).map(_.trim).filter(_.nonEmpty)

  /** Server options that go with [[sourcepath]]. */
  def sourcepathServerOpts: Seq[String] =
    if (sourcepath)
      Seq("--sourcepath") ++ sourcepathMode.map(mode => s"--sourcepath-mode=$mode").toSeq
    else
      Nil

}
