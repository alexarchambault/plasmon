package plasmon.command

import caseapp.core.RemainingArgs
import coursier.jvm.Execve
import plasmon.internal.{BinaryName, Constants, Directories}

import java.io.File
import java.nio.file.{Files, Path, Paths}

import scala.util.Properties

object Code extends caseapp.Command[CodeOptions] {
  override def stopAtFirstUnrecognized = true
  def run(options: CodeOptions, remainingArgs: RemainingArgs): Unit = {

    val codeCommand = options.codeCommand
      .map(_.trim)
      .filter(_.nonEmpty)
      .getOrElse("code")

    val (extraArgs, _, hasExtension) =
      if (options.global) {
        val res           = os.proc(codeCommand, "--list-extensions").call()
        val hasExtension0 = res.out.lines().contains(extensionOrgName)
        (Nil, Map.empty[String, String], hasExtension0)
      }
      else {
        val dataDir           = os.Path((new Directories).dataDir(), os.pwd)
        val codeDir           = dataDir / "code"
        val codeUserDataDir   = codeDir / "user"
        val codeExtensionsDir = codeDir / "extensions"

        val serverDependency =
          s"${Constants.organization}:${Constants.moduleName}:${Constants.version}"

        val extraArgs0 = Seq[os.Shellable](
          "--user-data-dir",
          codeUserDataDir,
          "--extensions-dir",
          codeExtensionsDir,
          "--disable-extension",
          metalsExtensionOrgName
        )
        val extraEnv0 =
          Map("PLASMON_SERVER_DEPENDENCY" -> serverDependency) ++
            BinaryName.pathOpt.map("PLASMON_BINARY" -> _.toString).toSeq
        val hasExtension0 =
          os.isDir(codeExtensionsDir) &&
          os.list(codeExtensionsDir)
            .iterator
            .filter(_.last.startsWith(extensionOrgName + "-"))
            .filter(os.isDir)
            .find(_ => true)
            .isDefined

        (extraArgs0, extraEnv0, hasExtension0)
      }

    if (!hasExtension)
      os.proc(codeCommand, extraArgs, "--install-extension", vsix, "--force")
        .call(stdin = os.Inherit, stdout = os.Inherit)

    val allArgs = extraArgs.flatMap(_.value) ++ remainingArgs.all

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
        sys.env.toArray.sorted.map { case (k, v) => s"$k=$v" }
      )
      sys.error("should not happen")
    }
    else {
      val res = os.proc(codeCommand, allArgs)
        .call(stdin = os.Inherit, stdout = os.Inherit, check = false)
      if (res.exitCode != 0)
        sys.exit(res.exitCode)
    }
  }

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

  private lazy val vsix =
    os.temp(
      os.read.bytes(os.resource / "plasmon/plasmon.vsix"),
      prefix = "plasmon",
      suffix = ".vsix"
    )
}
