package plasmon.command

import caseapp.core.RemainingArgs
import plasmon.internal.BuildVersion

object Version extends caseapp.Command[VersionOptions] {
  def run(options: VersionOptions, remainingArgs: RemainingArgs): Unit = {
    if (remainingArgs.all.nonEmpty)
      sys.error(
        s"Expected no argument, got ${remainingArgs.all.length} (${remainingArgs.all.mkString(", ")})"
      )

    if (options.commitHash)
      println(BuildVersion.commitHash.getOrElse("unknown"))
    else
      println(s"plasmon ${BuildVersion.fullVersion}")
  }
}
