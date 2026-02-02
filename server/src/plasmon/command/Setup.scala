package plasmon.command

import caseapp.core.RemainingArgs
import plasmon.internal.{BinaryName, Constants}

import java.nio.charset.StandardCharsets

import scala.collection.mutable.ListBuffer
import scala.meta.internal.metals.JdkVersion0

object Setup extends caseapp.Command[SetupOptions] {
  def run(options: SetupOptions, remainingArgs: RemainingArgs): Unit = {
    val dir = remainingArgs.all match {
      case Seq()    => os.pwd
      case Seq(str) => os.Path(str, os.pwd)
      case other =>
        sys.error(
          s"Expected a single directory as argument, got ${other.length} (${other.mkString(", ")})"
        )
    }
    val settingsFile   = dir / ".vscode/settings.json"
    val currentContent = readCurrentContent(settingsFile, options.verbosity)

    val updates = new ListBuffer[(String, String)]

    val currentBinaryOpt = currentContent.get("metals.metalsBinary").map(_.str)
    (BinaryName.pathOpt, currentBinaryOpt) match {
      case (Some(binary), Some(value)) =>
        if (binary.toString != value) {
          updates += "metals.metalsBinary"      -> binary.toString
          updates += "plasmon.bak.metalsBinary" -> value
        }
      case (Some(binary), None) =>
        updates += "metals.metalsBinary"      -> binary.toString
        updates += "plasmon.bak.metalsBinary" -> ""
      case (None, Some(value)) =>
        updates += "metals.metalsBinary"      -> "undefined"
        updates += "plasmon.bak.metalsBinary" -> value
      case (None, None) =>
    }

    lazy val javaHome: os.Path = {
      val cache        = coursierapi.Cache.create().withLogger(coursierapi.Logger.progressBars())
      val archiveCache = coursierapi.ArchiveCache.create().withCache(cache)
      os.Path(coursierapi.JvmManager.create().setArchiveCache(archiveCache).get("17"), os.pwd)
    }
    val currentJavaHomeOpt = currentContent.get("metals.javaHome").map(_.str)
    currentJavaHomeOpt match {
      case Some(currentJavaHome) =>
        val isAtLeastJava17 = JdkVersion0.fromJavaHome(os.Path(currentJavaHome, os.pwd).toNIO)
          .exists(_.major >= 17)
        if (!isAtLeastJava17) {
          updates += "metals.javaHome"      -> javaHome.toString
          updates += "plasmon.bak.javaHome" -> currentJavaHome
        }
      case None =>
        // FIXME The default system JVM may already be >= 17 and be fine for us
        updates += "metals.javaHome"      -> javaHome.toString
        updates += "plasmon.bak.javaHome" -> ""
    }

    val currentServerVersionOpt = currentContent.get("metals.serverVersion").map(_.str)
    val serverVersionValue =
      s"${Constants.organization}:${Constants.moduleName}:${Constants.version}"
    currentServerVersionOpt match {
      case Some(currentServerVersion) =>
        if (currentServerVersion != serverVersionValue) {
          updates += "metals.serverVersion"      -> serverVersionValue
          updates += "plasmon.bak.serverVersion" -> currentServerVersion
        }
      case None =>
        updates += "metals.serverVersion"      -> serverVersionValue
        updates += "plasmon.bak.serverVersion" -> ""
    }

    writeUpdates(settingsFile, updates.toList, options.verbosity)
  }

  private lazy val settingsEditJs =
    os.temp(
      os.read.bytes(os.resource / "plasmon/settingsEdit.js"),
      prefix = "settingsEdit",
      suffix = ".js"
    )

  def stripComments(input: os.Path): Array[Byte] =
    os.proc("node", settingsEditJs, "strip-comments", input)
      .call()
      .out.bytes
  def applyUpdates(input: os.Path, updates: Seq[(String, String)]): Array[Byte] =
    os.proc("node", settingsEditJs, "update", input, updates.map { case (k, v) => s"$k:$v" })
      .call()
      .out.bytes

  def readCurrentContent(settingsFile: os.Path, verbosity: Int) = {

    val settingsFileExists = os.isFile(settingsFile)
    val currentContent =
      if (settingsFileExists) {
        val settingsAsJson = stripComments(settingsFile)
        if (verbosity >= 1)
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

    if (verbosity >= 1)
      pprint.err.log(currentContent)

    currentContent
  }

  def writeUpdates(settingsFile: os.Path, updates: Seq[(String, String)], verbosity: Int): Unit = {

    if (verbosity >= 1)
      pprint.err.log(updates)

    if (updates.nonEmpty)
      if (os.isFile(settingsFile)) {
        val updatedContent = applyUpdates(settingsFile, updates.toList.sorted)
        System.err.println(s"Updating $settingsFile")
        os.write.over(settingsFile, updatedContent)
      }
      else {
        val keyValues = updates.toList.sorted.map { case (k, v) => k -> ujson.Str(v) }
        val content = ujson.Obj(keyValues.head, keyValues.tail*)
          .render(indent = 2)
        System.err.println(s"Writing $settingsFile")
        os.write(settingsFile, content, createFolders = true)
      }
  }
}
