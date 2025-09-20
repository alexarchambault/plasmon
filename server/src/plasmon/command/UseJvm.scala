package plasmon.command

import caseapp.core.RemainingArgs

object UseJvm extends caseapp.Command[UseJvmOptions] {
  override def names = List(
    List("use-jvm"),
    List("use", "jvm")
  )
  def run(options: UseJvmOptions, remainingArgs: RemainingArgs): Unit = {
    val dir = remainingArgs.all match {
      case Seq()    => os.pwd
      case Seq(str) => os.Path(str, os.pwd)
      case other =>
        sys.error(
          s"Expected a single directory as argument, got ${other.length} (${other.mkString(", ")})"
        )
    }
    val settingsFile = dir / ".vscode/settings.json"
    Setup.readCurrentContent(settingsFile, options.verbosity)

    Setup.writeUpdates(settingsFile, Seq("plasmon.useJvm" -> "true"), options.verbosity)
  }
}
