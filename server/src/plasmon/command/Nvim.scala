package plasmon.command

import caseapp.core.RemainingArgs
import coursier.jvm.Execve
import plasmon.internal.{Constants, Directories}

import scala.util.Properties

/** Runs Neovim with the Plasmon plugin, on the current directory or on the one passed.
  *
  * Neovim runs isolated from the user's own set-up: the XDG_* directories it reads its config from
  * and keeps its data, state and cache in point to Plasmon's own directories. Its config is either
  * LazyVim with the Plasmon plugin (default), or a minimal one with the Plasmon plugin alone. Both
  * live in our resources, along with the plugin, and get written under Plasmon's data directory.
  * The Plasmon server Neovim starts is run by the same plasmon binary as us.
  */
object Nvim extends caseapp.Command[NvimOptions] {
  override def names                   = List(List("nvim"), List("neovim"))
  override def stopAtFirstUnrecognized = true

  def run(options: NvimOptions, remainingArgs: RemainingArgs): Unit = {

    val (cwd, nvimArgs) = remainingArgs.all match {
      case Seq(first, rest*) if !first.startsWith("-") && os.isDir(os.Path(first, os.pwd)) =>
        (os.Path(first, os.pwd), rest)
      case other =>
        (os.pwd, other)
    }

    val nvim = nvimBinary(options.neovim.map(_.trim).filter(_.nonEmpty))

    val dirs      = new Directories
    val dataDir   = os.Path(dirs.dataDir(), os.pwd) / "neovim"
    val pluginDir = dataDir / "plugin"
    writePlugin(pluginDir)

    val serverCmd = AutoServer.selfCommand().getOrElse {
      System.err.println("Cannot work out how to start a Plasmon server from here")
      sys.exit(1)
    } :+ "server"

    // What we point the XDG_* variables to, for Neovim. The server it starts gets the former values
    // back, so that it (and the build tools it runs) find their caches and config where they
    // usually do.
    val xdgDirs = Seq(
      "XDG_CONFIG_HOME" -> dataDir / "config",
      "XDG_DATA_HOME"   -> dataDir / "data",
      "XDG_STATE_HOME"  -> dataDir / "state",
      "XDG_CACHE_HOME"  -> os.Path(dirs.cacheDir(), os.pwd) / "neovim"
    )
    val serverEnv = xdgDirs.flatMap {
      case (name, _) =>
        sys.env.get(name).orElse(defaultXdgDir(name)).map(name -> _)
    }

    val config = if (options.lazyvim) "lazyvim" else "init"
    val env = sys.env ++ xdgDirs.map { case (k, v) => k -> v.toString } ++ Seq(
      "NVIM_APPNAME"            -> (if (options.lazyvim) "lazyvim" else "minimal"),
      "PLASMON_NVIM_SERVER_CMD" -> ujson.write(ujson.Arr.from(serverCmd)),
      "PLASMON_NVIM_SERVER_ENV" -> ujson.write(ujson.Obj.from(serverEnv.map {
        case (k, v) => k -> ujson.Str(v)
      }))
    )
    // With execve, nvim starts in our working directory: have it change directory first thing
    // (the plugin picks the workspace from it)
    val cdArgs =
      if (cwd == os.pwd || !Execve.available()) Nil
      else Seq("--cmd", s"exe 'cd' fnameescape('${cwd.toString.replace("'", "''")}')")
    val args = cdArgs ++ Seq("-u", (pluginDir / "config" / s"$config.lua").toString) ++ nvimArgs

    if (Execve.available()) {
      Execve.execve(
        nvim.toString,
        (nvim.last +: args).toArray,
        env.toArray.sorted.map { case (k, v) => s"$k=$v" }
      )
      sys.error("should not happen")
    }
    else {
      val res = os.proc(nvim, args).call(
        cwd = cwd,
        env = env,
        propagateEnv = false,
        stdin = os.Inherit,
        stdout = os.Inherit,
        stderr = os.Inherit,
        check = false
      )
      if (res.exitCode != 0)
        sys.exit(res.exitCode)
    }
  }

  /** Where XDG-compliant programs put things when the variable is not set */
  private def defaultXdgDir(name: String): Option[String] =
    if (Properties.isLinux || Properties.isMac) {
      val home = os.home
      name match {
        case "XDG_CONFIG_HOME" => Some((home / ".config").toString)
        case "XDG_DATA_HOME"   => Some((home / ".local/share").toString)
        case "XDG_STATE_HOME"  => Some((home / ".local/state").toString)
        case "XDG_CACHE_HOME"  => Some((home / ".cache").toString)
        case _                 => None
      }
    }
    else
      // On Windows, XDG_* variables are not a thing, programs don't look at them. Neovim does, and
      // leaving ours in the server environment is harmless.
      None

  private val versionRegex = "\\d+\\.\\d+.*".r

  private def nvimBinary(neovimOpt: Option[String]): os.Path =
    neovimOpt match {
      case Some("system") =>
        Code.findInPath("nvim").map(os.Path(_)).getOrElse {
          System.err.println("nvim not found on PATH")
          sys.exit(1)
        }
      case Some(version @ versionRegex()) =>
        downloadNvim(version)
      case Some(path) =>
        val path0 = os.Path(path, os.pwd)
        if (!os.isFile(path0)) {
          System.err.println(s"$path0 not found")
          sys.exit(1)
        }
        path0
      case None =>
        downloadNvim(Constants.neovimVersion)
    }

  private def downloadNvim(version: String): os.Path = {
    val isArm = Option(System.getProperty("os.arch")).exists(arch =>
      arch == "aarch64" || arch == "arm64"
    )
    val archiveName =
      if (Properties.isWin) "nvim-win64.zip"
      else if (Properties.isMac)
        if (isArm) "nvim-macos-arm64.tar.gz" else "nvim-macos-x86_64.tar.gz"
      else if (isArm) "nvim-linux-arm64.tar.gz"
      else "nvim-linux-x86_64.tar.gz"
    val url = s"https://github.com/neovim/neovim/releases/download/v$version/$archiveName"
    val dir =
      try os.Path(coursierapi.ArchiveCache.create().get(coursierapi.Artifact.of(url)), os.pwd)
      catch {
        case e: Exception =>
          System.err.println(s"Error downloading Neovim $version from $url: ${e.getMessage}")
          sys.exit(1)
      }
    val exe = if (Properties.isWin) "nvim.exe" else "nvim"
    os.walk(dir, maxDepth = 3)
      .find(p => p.last == exe && (p / os.up).last == "bin")
      .getOrElse {
        System.err.println(s"No $exe found under $dir")
        sys.exit(1)
      }
  }

  private def resourcePrefix = "plasmon/neovim"

  private def readResource(path: String): Array[Byte] = {
    val is = Option(Nvim.getClass.getClassLoader.getResourceAsStream(path)).getOrElse {
      sys.error(s"Resource $path not found")
    }
    try is.readAllBytes()
    finally is.close()
  }

  /** Writes the plugin and the configs from our resources to `dir`, leaving files that are already
    * up-to-date alone, and removing those that we don't have anymore.
    */
  private def writePlugin(dir: os.Path): Unit = {
    val files = new String(readResource(s"$resourcePrefix/files.txt"), "UTF-8")
      .linesIterator
      .map(_.trim)
      .filter(_.nonEmpty)
      .toVector
    val expected = files.map(os.SubPath(_)).toSet
    for (sub <- files.map(os.SubPath(_))) {
      val content = readResource(s"$resourcePrefix/$sub")
      val dest    = dir / sub
      if (!os.isFile(dest) || !java.util.Arrays.equals(os.read.bytes(dest), content))
        os.write.over(dest, content, createFolders = true)
    }
    if (os.isDir(dir))
      for (path <- os.walk(dir).filter(os.isFile) if !expected(path.subRelativeTo(dir)))
        os.remove(path)
  }
}
