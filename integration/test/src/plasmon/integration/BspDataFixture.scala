package plasmon.integration

import java.io.OutputStream

/** Committed recordings of what the build tools report over BSP.
  *
  * These serve two purposes at once:
  *   - they are what the build tool tests assert on - the whole point of those tests is that Mill,
  *     sbt and Scala CLI keep describing a project the way we expect;
  *   - they are what the presentation compiler tests are fed, so that those don't have to run a
  *     build tool at all.
  *
  * The server writes them under `.plasmon/cache/bsp` during an import, with absolute paths swapped
  * for placeholders (see `plasmon.bsp.BspDataPortability`) and JDK homes collapsed to
  * `$JAVA_HOME_URI:<major>$`, which is what makes them committable and replayable on another
  * machine or OS.
  */
object BspDataFixture {

  /** Name of the directory a recording is installed under in a workspace.
    *
    * Must match `plasmon.bsp.BuildTool.Replay.dirName`.
    */
  private def replayDirName = ".plasmon-replay"

  /** Discovery / build tool id of the replay build tool, i.e. `plasmon.bsp.BuildTool.Replay.id`. */
  def replayBuildToolId = "replay"

  /** A recording is complete once this is in it - the server refuses to replay without it. */
  private def markerFileName = "workspaceBuildTargets.json"

  def dir(key: os.SubPath): os.Path =
    TestUtil.fixtureDir / "plasmon/integration/bsp-data" / key

  /** The directory the server recorded into during this import.
    *
    * The layout underneath `.plasmon/cache/bsp` depends on which build tool was loaded and where
    * its workspace sits, so rather than reproducing that here we look for the one directory holding
    * a recording. Test workspaces only ever load a single build tool.
    */
  private def recordedDir(workspace: os.Path): os.Path = {
    val cacheDir = workspace / ".plasmon/cache/bsp"
    if (!os.isDir(cacheDir))
      sys.error(s"No BSP data recorded under $cacheDir")
    val candidates = os.walk(cacheDir)
      .filter(path => os.isFile(path) && path.last == markerFileName)
      .map(_ / os.up)
    candidates match {
      case Seq(dir) => dir
      case Seq() =>
        sys.error(s"No BSP data recorded under $cacheDir (no $markerFileName found)")
      case several =>
        sys.error(
          s"Several BSP recordings found under $cacheDir: ${several.mkString(", ")}. " +
            "A test workspace is expected to load exactly one build tool."
        )
    }
  }

  /** Copies what the server just recorded into the committed fixtures. */
  def record(workspace: os.Path, key: os.SubPath, osOpt: Option[OutputStream]): Unit = {
    val from = recordedDir(workspace)
    val to   = dir(key)
    val err  = osOpt.getOrElse(System.err)
    err.write(s"Recording BSP data to $to${System.lineSeparator()}".getBytes("UTF-8"))
    err.flush()
    os.remove.all(to)
    os.makeDir.all(to)
    for (f <- os.list(from) if os.isFile(f))
      os.copy.over(f, to / f.last)

    // Recorded entries can name a file or a whole generated source directory
    for {
      rel <- generatedSourcePaths(to)
      src = workspace / rel
      file <-
        if (os.isDir(src)) os.walk(src).filter(os.isFile)
        else if (os.isFile(src)) Seq(src)
        else Nil
      if isSourceFile(file)
    } os.copy.over(
      file,
      to / generatedSourcesDirName / file.subRelativeTo(workspace),
      createFolders = true
    )
  }

  private def isSourceFile(path: os.Path): Boolean =
    path.last.endsWith(".scala") || path.last.endsWith(".java") || path.last.endsWith(".sc")

  private def generatedSourcesDirName = "generated-sources"

  /** Workspace-relative paths of the sources the build tool generates.
    *
    * Unlike compiled output these are plain text, so they are committed as-is rather than rebuilt -
    * nothing but the build tool could produce them. Scala CLI's script wrappers are the case that
    * matters; Mill's wrappers around `build.mill` come along for the ride.
    */
  private def generatedSourcePaths(recordingDir: os.Path): Seq[os.SubPath] = {
    def read(name: String): Option[ujson.Value] = {
      val f = recordingDir / s"$name.json"
      if (os.exists(f)) Some(ujson.read(os.read(f))) else None
    }
    def items(v: ujson.Value): Seq[ujson.Value] =
      v.obj.get("items").toSeq.flatMap(_.arr)

    val fromSources = read("buildTargetSources").toSeq
      .flatMap(items)
      .flatMap(_.obj.get("sources").toSeq.flatMap(_.arr))
      .filter(_.obj.get("generated").exists(_.bool))
      .flatMap(_.obj.get("uri").map(_.str))

    val fromWrapped = read("buildTargetWrappedSources").toSeq
      .flatMap(items)
      .flatMap(_.obj.get("sources").toSeq.flatMap(_.arr))
      .flatMap(_.obj.get("generatedUri").map(_.str))

    (fromSources ++ fromWrapped).distinct.flatMap(workspaceRelative)
  }

  /** Turns a recorded `$WORKSPACE$`-rooted URI into a workspace-relative path, if it is one. */
  private def workspaceRelative(uri: String): Option[os.SubPath] = {
    val prefixes = Seq("file:///$WORKSPACE$/", "file:/$WORKSPACE$/")
    prefixes.collectFirst {
      case p if uri.startsWith(p) => uri.stripPrefix(p)
    }
      .map(_.takeWhile(c => c != '?' && c != '#'))
      .filter(_.nonEmpty)
      .map(os.SubPath(_))
  }

  /** Checks what the server just recorded against committed fixtures.
    *
    * This is the assertion the build tool tests are built around: the recording is a full,
    * machine-independent description of what Mill / sbt / Scala CLI told us about a project, so
    * comparing it catches a build tool changing its class paths, its source layout, its Scala
    * version handling or its target names.
    *
    * Follows the usual snapshot convention - regenerated on CI, where the workflow's `Diff` step
    * turns any change into a failure, and compared locally.
    */
  def check(workspace: os.Path, key: os.SubPath, osOpt: Option[OutputStream]): Unit = {
    val from = recordedDir(workspace)
    val to   = dir(key)

    val recorded = os.list(from).filter(os.isFile).map(_.last).sorted

    if (TestParams.updateSnapshots) {
      val expected =
        if (os.isDir(to)) os.list(to).filter(os.isFile).map(_.last).toSet else Set.empty
      for (name <- expected -- recorded.toSet)
        os.remove(to / name)
    }
    else {
      if (!os.isDir(to))
        sys.error(
          s"No recorded BSP data at $to. Run the test with PLASMON_RECORD_BSP_DATA=true to record it."
        )
      val expected = os.list(to).filter(os.isFile).map(_.last).sorted
      assert(
        recorded == expected,
        s"Recorded BSP responses $recorded do not match the fixtures $expected in $to"
      )
    }

    for (name <- recorded)
      TestUtil.checkTextFixture(to / name, scrub(os.read(from / name)), osOpt)
  }

  /** sbt names the wrapper object of a generated build definition after a per-session hash, so it
    * differs between two runs over the same project. It says nothing about the project, and these
    * fixtures are only ever compared - never replayed - so it is blanked out rather than committed.
    */
  private def scrub(content: String): String =
    content.replaceAll("""\$[0-9a-f]{16,}""", """\$<hash>""")

  /** Installs a committed recording into a workspace, where the replay build tool picks it up. */
  def install(workspace: os.Path, key: os.SubPath): Unit = {
    val from = dir(key)
    if (!os.isDir(from) || !os.exists(from / markerFileName))
      sys.error(
        s"No recorded BSP data at $from. Run the test with PLASMON_RECORD_BSP_DATA=true to record it."
      )
    val to = workspace / replayDirName
    os.makeDir.all(to)
    for (f <- os.list(from) if os.isFile(f))
      os.copy.over(f, to / f.last)

    // Put back the sources only the build tool could have produced, where it would have put them
    val generated = from / generatedSourcesDirName
    if (os.isDir(generated))
      for (f <- os.walk(generated) if os.isFile(f))
        os.copy.over(f, workspace / f.subRelativeTo(generated), createFolders = true)
  }

  // Note on what replay can't do: it rebuilds class directories by compiling recorded sources, but
  // it cannot produce sources a build tool *generates*. That is fine for the wrappers Mill emits for
  // build.mill - no test navigates those - but not for Scala CLI's script mode, where the module's
  // own sources are generated wrappers around the .sc files. Script-based tests therefore keep
  // running the real build tool; see the scriptBased check in BasicTests. Tempting as it is to catch
  // this automatically, "the recording declares generated sources" also matches Mill's meta-build,
  // which replays fine, so it would reject working setups.
}
