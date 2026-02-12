package plasmon.command

import caseapp.core.RemainingArgs

import java.nio.charset.StandardCharsets

object Revert extends caseapp.Command[RevertOptions] {
  def run(options: RevertOptions, remainingArgs: RemainingArgs): Unit = {
    val dir = remainingArgs.all match {
      case Seq()    => os.pwd
      case Seq(str) => os.Path(str, os.pwd)
      case other =>
        sys.error(
          s"Expected a single directory as argument, got ${other.length} (${other.mkString(", ")})"
        )
    }
    val settingsFile       = dir / ".vscode/settings.json"
    val settingsFileExists = os.isFile(settingsFile)
    if (settingsFileExists) {
      val currentContent =
        if (settingsFileExists) {
          val settingsAsJson = Setup.stripComments(settingsFile)
          if (options.verbosity >= 1)
            pprint.err.log(new String(settingsAsJson, StandardCharsets.UTF_8))
          ujson.read(settingsAsJson).obj
        }
        else {
          if (os.exists(settingsFile))
            System.err.println(
              s"Warning: $settingsFile exists but doesn't seem to be a file, try to proceed anyway…"
            )
          ujson.Obj().obj
        }

      if (options.verbosity >= 1)
        pprint.err.log(currentContent)

      val updates0 =
        for {
          key    <- Seq("metalsBinary", "javaHome", "serverVersion")
          binary <- currentContent.get(s"plasmon.bak.$key").map(_.str)
        } yield Seq(
          s"metals.$key"      -> (if (binary.isEmpty) "undefined" else binary),
          s"plasmon.bak.$key" -> "undefined"
        )
      val updates = updates0.flatten

      if (options.verbosity >= 1)
        pprint.err.log(updates)

      if (updates.nonEmpty)
        if (settingsFileExists) {
          val updatedContent = Setup.applyUpdates(settingsFile, updates.sorted.toList)
          System.err.println(s"Updating $settingsFile")
          os.write.over(settingsFile, updatedContent)
        }
        else {
          val keyValues = updates.sorted.toList.map { case (k, v) => k -> ujson.Str(v) }
          val content = ujson.Obj(keyValues.head, keyValues.tail*)
            .render(indent = 2)
          System.err.println(s"Writing $settingsFile")
          os.write(settingsFile, content, createFolders = true)
        }
    }
    else
      System.err.println(s"$settingsFile not found, nothing to revert")
  }
}
