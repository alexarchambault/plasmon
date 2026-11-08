package plasmon.command

import caseapp.core.RemainingArgs
import coursier.jvm.Execve
import plasmon.internal.{BinaryName, Directories}

import java.io.File
import java.nio.file.{Files, Path, Paths}
import java.security.MessageDigest

import scala.util.Properties

object Code extends caseapp.Command[CodeOptions] {
  override def stopAtFirstUnrecognized = true
  def run(options: CodeOptions, remainingArgs: RemainingArgs): Unit = {

    val codeCommand = options.codeCommand
      .map(_.trim)
      .filter(_.nonEmpty)
      .getOrElse("code")

    val dataDir = os.Path((new Directories).dataDir(), os.pwd)
    val codeDir = dataDir / "code"

    val vsixBytes = os.read.bytes(os.resource / "plasmon" / "plasmon.vsix")
    val vsixHash  = sha256(vsixBytes)

    // code-server accepts most of the code CLI options, but not --disable-extension
    val isCodeServer = os.FilePath(codeCommand).last.startsWith("code-server")

    // dirArgs: passed to all code invocations
    // launchArgs: passed only when opening the editor
    val (dirArgs, launchArgs, extensionMarker, hasExtension) =
      if (options.global) {
        val res = os.proc(codeCommand, "--list-extensions").call()
        val hasExtension0 =
          res.out.lines().map(_.trim).exists(_.equalsIgnoreCase(extensionOrgName))
        (Nil, Nil, codeDir / "global-extension.sha256", hasExtension0)
      }
      else {
        val codeUserDataDir   = codeDir / "user"
        val codeExtensionsDir = codeDir / "extensions"

        val dirArgs0 = Seq(
          "--user-data-dir",
          codeUserDataDir.toString,
          "--extensions-dir",
          codeExtensionsDir.toString
        )
        val launchArgs0 =
          if (isCodeServer) Nil
          else Seq("--disable-extension", metalsExtensionOrgName)
        val hasExtension0 =
          os.isDir(codeExtensionsDir) &&
          os.list(codeExtensionsDir)
            .iterator
            .filter(_.last.startsWith(extensionOrgName + "-"))
            .exists(os.isDir)

        (dirArgs0, launchArgs0, codeDir / "extension.sha256", hasExtension0)
      }

    // The extension embedded in this launcher may differ from the installed one even if their
    // versions match, so we keep track of what we installed via a hash of the VSIX file
    val upToDate = hasExtension &&
      os.isFile(extensionMarker) &&
      os.read(extensionMarker).trim == vsixHash

    if (!upToDate) {
      val vsix = codeDir / s"plasmon-$vsixHash.vsix"
      os.write.over(vsix, vsixBytes, createFolders = true)
      try
        os.proc(codeCommand, dirArgs, "--install-extension", vsix, "--force")
          .call(stdin = os.Inherit, stdout = os.Inherit)
      finally
        os.remove(vsix)
      os.write.over(extensionMarker, vsixHash, createFolders = true)
    }

    // dropping the "--" separating our options from those of code, if any
    // (as we stop at the first unrecognized argument, it ends up in remainingArgs.remaining)
    val userArgs = remainingArgs.remaining ++ remainingArgs.unparsed match {
      case "--" +: rest => rest
      case other        => other
    }
    val allArgs = dirArgs ++ launchArgs ++ userArgs

    val env = {
      val selfEnv0 = selfEnv()
      if (selfEnv0.isEmpty) sys.env
      else sys.env -- forcedEnvVars ++ selfEnv0
    }

    if (Execve.available()) {
      val (fullPath, name) =
        if (codeCommand.contains("/") || codeCommand.contains(File.separator)) {
          val path = os.Path(codeCommand, os.pwd)
          (path.toString, path.last)
        }
        else
          (
            findInPath(codeCommand).fold(codeCommand)(_.toString),
            codeCommand
          )
      Execve.execve(
        fullPath,
        name +: allArgs.toArray,
        env.toArray.sorted.map { case (k, v) => s"$k=$v" }
      )
      sys.error("should not happen")
    }
    else {
      val res = os.proc(codeCommand, allArgs)
        .call(
          stdin = os.Inherit,
          stdout = os.Inherit,
          check = false,
          env = env,
          propagateEnv = false
        )
      if (res.exitCode != 0)
        sys.exit(res.exitCode)
    }
  }

  /** Environment variables the extension reads to know how to start the plasmon server */
  private def forcedEnvVars = Set(
    "PLASMON_FORCED_COMMAND",
    "PLASMON_FORCED_BINARY",
    "PLASMON_FORCED_VERSION"
  )

  /** Environment variables telling the extension to start the plasmon server with the very plasmon
    * launcher running this, rather than whatever 'plasmon' is in the PATH
    */
  private def selfEnv(): Map[String, String] =
    BinaryName.pathOpt match {
      case Some(binary) =>
        Map("PLASMON_FORCED_BINARY" -> binary.toString)
      case None =>
        AutoServer.selfCommand() match {
          case Some(command) =>
            Map("PLASMON_FORCED_COMMAND" -> ujson.write(ujson.Arr(command.map(ujson.Str(_))*)))
          case None =>
            System.err.println(
              "Warning: cannot work out how to start plasmon from here, " +
                "the extension will use the plasmon from the PATH"
            )
            Map.empty
        }
    }

  private def sha256(bytes: Array[Byte]): String =
    MessageDigest.getInstance("SHA-256")
      .digest(bytes)
      .map(b => f"${b & 0xff}%02x")
      .mkString

  // from https://github.com/VirtusLab/scala-cli/blob/b92d86a02e741a3cea9cc943f59712a5fb385617/modules/build/src/main/scala/scala/build/internal/Runner.scala#L175-L202
  private def endsWithCaseInsensitive(s: String, suffix: String): Boolean =
    s.length >= suffix.length &&
    s.regionMatches(true, s.length - suffix.length, suffix, 0, suffix.length)
  private def findInPath(app: String): Option[Path] = {
    val asIs = Paths.get(app)
    if (Paths.get(app).getNameCount >= 2) Some(asIs)
    else {
      def pathEntries =
        Option(System.getenv("PATH"))
          .iterator
          .flatMap(_.split(File.pathSeparator).iterator)
      def pathSep =
        if (Properties.isWin)
          Option(System.getenv("PATHEXT"))
            .iterator
            .flatMap(_.split(File.pathSeparator).iterator)
        else Iterator("")
      def matches = for {
        dir <- pathEntries
        ext <- pathSep
        app0 = if (endsWithCaseInsensitive(app, ext)) app else app + ext
        path = Paths.get(dir).resolve(app0)
        if Files.isExecutable(path)
      } yield path
      matches.take(1).toList.headOption
    }
  }

  private def extensionOrgName       = "alexarchambault.plasmon"
  private def metalsExtensionOrgName = "scalameta.metals"
}
