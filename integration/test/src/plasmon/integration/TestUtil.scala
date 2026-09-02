package plasmon.integration

import com.eed3si9n.expecty.Expecty.expect
import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import com.google.gson.{Gson, GsonBuilder, JsonSyntaxException}
import coursier.cache.FileCache
import io.github.alexarchambault.testutil.OutputFrame
import io.github.alexarchambault.testutil.TestOutput.FixedReadBytes
import io.github.alexarchambault.testutil.TestUtil.*
import org.eclipse.lsp4j as l
import org.eclipse.lsp4j.jsonrpc.Launcher
import org.eclipse.lsp4j.services.LanguageServer

import java.io.{OutputStream, PrintStream}
import java.net.URI
import java.nio.charset.StandardCharsets
import java.nio.file.Paths
import java.util.{Arrays, Locale}
import java.util.concurrent.*

import scala.annotation.nowarn
import scala.concurrent.duration.{Duration, DurationInt, FiniteDuration}
import scala.jdk.CollectionConverters.*
import scala.reflect.ClassTag
import scala.util.Properties
import scala.util.control.NonFatal

object TestUtil {

  val pool: ExecutorService = fixedThreadPool("plasmon-tests", 4)

  val launcher = sys.props.getOrElse(
    "plasmon.integration.launcher",
    sys.error("Java property plasmon.integration.launcher not set")
  )

  val launcherKind = sys.props.getOrElse(
    "plasmon.integration.launcher-kind",
    sys.error("Java property plasmon.integration.launcher-kind not set")
  )

  lazy val fixtureDir =
    sys.props
      .get("plasmon.integration.fixture-dir")
      .map(os.Path(_, os.pwd))
      .getOrElse {
        sys.error("plasmon.integration.fixture-dir not set")
      }

  lazy val generatedResourcesDir =
    sys.props
      .get("plasmon.integration.generated-resources-dir")
      .map(os.Path(_, os.pwd))
      .getOrElse {
        sys.error("plasmon.integration.generated-resources-dir not set")
      }

  lazy val disableScala2Pc =
    sys.props
      .get("plasmon.integration.disableScala2Pc")
      .map {
        case "true"  => true
        case "false" => false
        case other   => sys.error(s"Malformed plasmon.integration.disableScala2Pc value: '$other'")
      }
      .getOrElse {
        sys.error("plasmon.integration.disableScala2Pc not set")
      }

  lazy val baseTimeout = Option(System.getenv("PLASMON_TIMEOUT_OVERRIDE"))
    .map(Duration(_))
    .map {
      case f: FiniteDuration => f
      case other =>
        sys.error(s"PLASMON_TIMEOUT_OVERRIDE must be finite (got $other)")
    }
    .getOrElse(1.minute)

  def withWorkspaceServerPositionsCount[T](
    mode: TestMode = TestMode.Lsp,
    projectName: String = "test-project",
    client: l.services.LanguageClient = new MockLanguageClient {},
    clientCapabilities: l.ClientCapabilities = new l.ClientCapabilities,
    timeout: Option[FiniteDuration] = Some(baseTimeout),
    extraServerOpts: Seq[String] = Nil,
    count: Int = 1
  )(
    content: (os.SubPath, String)*
  )(f: (os.Path, ServerDriver, Positions, Option[OutputStream], Int) => T): T = {

    val pos = Positions.of(content*)
    val updatedContent = content.map {
      case (path, _) =>
        path -> (pos.content(path): os.Source)
    }

    withWorkspaceAndServer(
      mode,
      projectName,
      client,
      clientCapabilities,
      timeout = timeout,
      extraServerOpts = extraServerOpts,
      count = count
    )(updatedContent*) {
      (workspace, driver, osOpt, runCount) =>
        f(workspace, driver, pos, osOpt, runCount)
    }
  }

  def withWorkspaceServerPositions[T](
    mode: TestMode = TestMode.Lsp,
    projectName: String = "test-project",
    client: l.services.LanguageClient = new MockLanguageClient {},
    clientCapabilities: l.ClientCapabilities = new l.ClientCapabilities,
    timeout: Option[FiniteDuration] = Some(baseTimeout),
    extraServerOpts: Seq[String] = Nil,
    count: Int = 1
  )(
    content: (os.SubPath, String)*
  )(f: (os.Path, ServerDriver, Positions, Option[OutputStream]) => T): T =
    withWorkspaceServerPositionsCount(
      mode,
      projectName,
      client,
      clientCapabilities,
      timeout,
      extraServerOpts,
      count
    )(content*) {
      (workspace, driver, pos, osOpt, _) =>
        f(workspace, driver, pos, osOpt)
    }

  def serverExtraJavaOpts = Seq("-Duser.country=US", "-Duser.language=en")
  def serverEnv = Map(
    "PLASMON_JAVAC_EXTRA_OPTIONS" -> "-verbose",
    // Don't let a Mill output directory override from whoever runs the tests reach the Mill the
    // server spawns - it ends up in the paths Mill reports, and so in the recorded BSP data
    "MILL_OUTPUT_DIR" -> null,
    // Lets the server sort sbt's meta-build class path when writing recordings - see
    // BspDataPortability.sortSbtBootClasspath. Production never sets this.
    "PLASMON_SORT_SBT_BOOT_CLASSPATH" -> "true"
  )
  def withWorkspaceAndServer[T](
    mode: TestMode = TestMode.Lsp,
    projectName: String = "test-project",
    client: l.services.LanguageClient = new MockLanguageClient {},
    clientCapabilities: l.ClientCapabilities = new l.ClientCapabilities,
    shutdownServer: Boolean = true,
    timeout: Option[FiniteDuration] = Some(baseTimeout),
    extraServerOpts: Seq[String] = Nil,
    workspaceOpt: Option[os.Path] = None,
    count: Int = 1
  )(
    content: (os.SubPath, os.Source)*
  )(f: (os.Path, ServerDriver, Option[OutputStream], Int) => T): T = {

    val workingDir = os.sub / projectName

    val baseCommand: os.Shellable =
      if (launcherKind == "native") Seq[os.Shellable](launcher, serverExtraJavaOpts)
      else
        Seq[os.Shellable](
          "java",
          // Add JAVA_HOME in env too
          // s"-agentlib:native-image-agent=config-output-dir=..../native-config",
          "--add-opens=java.base/java.util=ALL-UNNAMED", // needed for TimerThreadsHack
          "--add-opens=jdk.compiler/com.sun.tools.javac.file=ALL-UNNAMED", // needed for Java compiler class path hacks
          serverExtraJavaOpts,
          "-jar",
          launcher
        )
    PlasmonProcessTest(
      os.proc(
        baseCommand,
        "server",
        "--log-to-stderr",
        "--scala-cli",
        scalaCli,
        // Nothing will connect to it over LSP: it initializes itself on its working directory,
        // which is the workspace, and answers on its command socket alone
        if (mode == TestMode.Cli) Seq("--lsp=false") else Nil,
        extraServerOpts
      ),
      timeout = timeout.map(_ * mode.timeoutFactor),
      count = count,
      env = serverEnv,
      runProcIn = tmpDir => {
        val dir = workspaceOpt.getOrElse(tmpDir / workingDir)
        os.makeDir.all(dir)
        dir
      },
      enableOutputFrame = TestParams.enableOutputFrame,
      enableSilentOutput = TestParams.enableSilentOutput,
      printOutputOnError = TestParams.printOutputOnError,
      cleanUp = TestParams.cleanUpAfterTests,
      newOutputFrame = () => new OutputFrame(widthShift = -7)
    )(
      content.map { case (p, s) => (workingDir / p, s) }*
    ) {
      (tmpDir, subProc, ignoreSubProcExit, output, runCount) =>
        val workspace   = workspaceOpt.getOrElse(tmpDir / workingDir)
        val printStream = TestLogs.printStream(output.printStream)
        val osOpt       = TestLogs.outputStream(output.outputStreamOpt)

        for (outputStream <- osOpt)
          client match {
            case client0: MockLanguageClient =>
              client0.setOutputStream(outputStream)
            case _ =>
          }

        def lspDriver(): ServerDriver.Lsp = {
          val jsonrpcLauncher = new Launcher.Builder[LanguageServer]()
            .setExecutorService(pool)
            .setInput(subProc.stdout.wrapped)
            .setOutput(subProc.stdin.wrapped)
            .setRemoteInterface(classOf[LanguageServer])
            .setLocalService(client)
            .setExceptionHandler { t =>
              printStream.println(s"Error during LSP processing: $t")
              t.printStackTrace(printStream)
              printStream.flush()
              l.jsonrpc.RemoteEndpoint.DEFAULT_EXCEPTION_HANDLER.apply(t)
            }
            .create()

          val remoteServer = jsonrpcLauncher.getRemoteProxy

          val listeningFuture = jsonrpcLauncher.startListening()

          remoteServer.initialize {
            val params = new l.InitializeParams
            params.setProcessId(subProc.wrapped.pid().toInt)
            @nowarn
            def deprecatedStuff(): Unit = {
              params.setRootPath(workspace.toNIO.toString)
              params.setRootUri(workspace.toNIO.toUri.toASCIIString)
            }
            deprecatedStuff()
            // params.setInitializationOptions(???)
            params.setCapabilities(clientCapabilities)
            params.setClientInfo(
              new l.ClientInfo("Plasmon integration", "0.1.0-SNAPSHOT")
            )
            params.setLocale("en")
            params.setTrace("off")
            params.setWorkspaceFolders(
              List(new l.WorkspaceFolder(workspace.toNIO.toUri.toASCIIString, workspace.last))
                .asJava
            )
            params
          }.get()

          ServerDriver.Lsp(remoteServer, workspace, client, listeningFuture)
        }

        def cliDriver(): ServerDriver.Cli = {
          val driver0 = ServerDriver.Cli(workspace, osOpt)
          // Nothing has waited for the server so far - no `initialize` was sent, and none is
          // coming
          driver0.awaitReady(baseTimeout)
          driver0
        }

        val driver: ServerDriver =
          mode match {
            case TestMode.Lsp => lspDriver()
            case TestMode.Cli => cliDriver()
          }

        try
          f(workspace, driver, osOpt, runCount)
        finally
          if (shutdownServer) {
            printStream.println("Trying to ignore sub-process exit")
            printStream.flush()
            ignoreSubProcExit()
            printStream.println("Stopping server")
            printStream.flush()
            driver.stopServer()
          }
    }
  }

  /** A workspace with `content` in it, and no server running.
    *
    * [[withWorkspaceAndServer]] starts one before the test body runs, which is the one thing a test
    * about a command starting a server of its own cannot have. Whatever server the body ends up
    * starting is stopped on the way out, whether it passed or failed.
    */
  def withWorkspaceNoServer[T](
    projectName: String = "test-project",
    timeout: Option[FiniteDuration] = Some(baseTimeout)
  )(
    content: (os.SubPath, String)*
  )(f: (os.Path, Positions) => T): T = {

    val positions  = Positions.of(content*)
    val workingDir = os.sub / projectName
    val files = content.map {
      case (path, _) => (workingDir / path, positions.content(path): os.Source)
    }

    val errorOutput = TestLogs.printStream(System.err)

    os.temp.withContent(files, TestParams.cleanUpAfterTests, errorOutput) { tmpDir =>
      val workspace = tmpDir / workingDir
      os.makeDir.all(workspace)
      try runWithTimeout(timeout)(f(workspace, positions))
      finally stopServerIfAny(workspace, errorOutput)
    }
  }

  /** Stops the server running in `workspace`, if one is - a test that leaves one behind leaves its
    * file watches behind too.
    */
  private def stopServerIfAny(workspace: os.Path, errorOutput: PrintStream): Unit =
    if (os.exists(workspace / ".plasmon/socket"))
      try runServerCommand(workspace, TestLogs.currentStream)("exit")
      catch {
        case NonFatal(e) =>
          errorOutput.println(s"Ignoring error stopping the server in $workspace: $e")
      }

  /** For the tests that are about the LSP connection itself rather than about what the server does.
    */
  def withLspServer[T](
    shutdownServer: Boolean = true,
    timeout: Option[FiniteDuration] = Some(baseTimeout)
  )(
    content: (os.SubPath, os.Source)*
  )(f: (os.Path, ServerDriver.Lsp, Option[OutputStream]) => T): T =
    withWorkspaceAndServer(
      TestMode.Lsp,
      shutdownServer = shutdownServer,
      timeout = timeout
    )(content*) {
      (workspace, driver, osOpt, _) =>
        driver match {
          case lsp: ServerDriver.Lsp => f(workspace, lsp, osOpt)
          case other                 => sys.error(s"Expected an LSP driver, got $other")
        }
    }

  /** The ways tests drive the server, from `PLASMON_TEST_MODES` (comma-separated ids).
    *
    * Both by default: every scenario is run once over LSP and once through the CLI. Narrow it down
    * with e.g. `PLASMON_TEST_MODES=lsp` when only one of the two entry points is of interest.
    */
  lazy val modes: Seq[TestMode] =
    Option(System.getenv("PLASMON_TEST_MODES"))
      .map(_.split(',').toSeq.map(_.trim).filter(_.nonEmpty))
      .map { ids =>
        ids.map { id =>
          TestMode.parse(id).getOrElse {
            sys.error(
              s"Unrecognized test mode '$id' in PLASMON_TEST_MODES " +
                s"(expected one of ${TestMode.all.map(_.id).mkString(", ")})"
            )
          }
        }
      }
      .getOrElse(TestMode.all)

  /** The modes a suite runs in when only the LSP entry point is worth exercising.
    *
    * [[modes]] minus the CLI: for a suite whose subject is server behaviour rather than how the
    * server is driven, a second run through the CLI costs more than it tells us.
    */
  lazy val lspOnlyModes: Seq[TestMode] =
    modes.filter(_ == TestMode.Lsp)

  /** The modes a suite runs in when only the CLI entry point is worth exercising.
    *
    * [[modes]] minus the LSP one: for a suite about something only `plasmon <command>` offers -
    * `--auto`, say - there is no LSP run to make.
    */
  lazy val cliOnlyModes: Seq[TestMode] =
    modes.filter(_ == TestMode.Cli)

  private val baseCommand: os.Shellable =
    if (launcherKind == "native") Seq[os.Shellable](launcher, serverExtraJavaOpts)
    else Seq[os.Shellable]("java", serverExtraJavaOpts, "-jar", launcher)

  /** Runs a `plasmon` command against the server running in `workspace`. */
  def runServerCommand(
    workspace: os.Path,
    err: Option[OutputStream],
    env: Map[String, String] = Map.empty
  )(command: os.Shellable*): Unit = {
    val output = FixedReadBytes.pipeTo(err)
    os.proc(baseCommand, "command", "-v", command)
      .call(
        cwd = workspace,
        env = env,
        stdin = os.Inherit,
        stdout = output,
        mergeErrIntoOut = err.nonEmpty
      )
  }

  def runCommand(
    workspace: os.Path,
    err: Option[OutputStream]
  )(command: os.Shellable*): Unit = {
    val output = FixedReadBytes.pipeTo(err)
    os.proc(command)
      .call(cwd = workspace, stdin = os.Inherit, stdout = output, mergeErrIntoOut = err.nonEmpty)
  }

  def serverCommandOutput(
    workspace: os.Path,
    err: Option[OutputStream],
    env: Map[String, String] = Map.empty
  )(command: os.Shellable*): String = {
    val output = FixedReadBytes.pipeTo(err)
    val proc = os.proc(baseCommand, "command", "-v", command)
      .call(cwd = workspace, env = env, stderr = output)
    proc.out.text()
  }

  def hoverMarkdown(
    driver: ServerDriver,
    path: os.Path,
    pos: l.Position
  ): String = {

    val hoverResp = driver.hover(path, pos)

    if (hoverResp == null) ""
    else {
      expect(hoverResp.getContents.isRight)
      expect(hoverResp.getContents.getRight.getKind == "markdown")

      val value = hoverResp.getContents.getRight.getValue

      if (System.lineSeparator() == "\n") value
      else
        value.linesIterator.zip(value.linesWithSeparators)
          .map {
            case (line, lineWithSep) =>
              if (lineWithSep.length > line.length)
                line + System.lineSeparator()
              else
                line
          }
          .mkString
    }
  }

  case class GoToDefResult(
    defPath: Either[os.Path, os.SubPath],
    startPos: (Int, Int),
    endPos: (Int, Int),
    content: String
  )

  private def definitionResultOf(workspace: os.Path, location: l.Location): DefinitionResult = {
    val defPath = os.Path(Paths.get(new URI(location.getUri)))

    val startPos = (location.getRange.getStart.getLine, location.getRange.getStart.getCharacter)
    val endPos   = (location.getRange.getEnd.getLine, location.getRange.getEnd.getCharacter)

    val content = os.read(defPath)
      .linesWithSeparators
      .zipWithIndex
      .map {
        case (line, idx) =>
          val line0 =
            if (idx == endPos._1) line.take(endPos._2)
            else line
          if (idx == startPos._1) line0.drop(startPos._2)
          else line0
      }
      .drop(startPos._1)
      .take(endPos._1 - startPos._1 + 1)
      .mkString

    val defPath0 =
      if (defPath.startsWith(workspace)) Right(defPath.relativeTo(workspace).asSubPath)
      else Left(defPath)
    DefinitionResult(GoToDefResult(defPath0, startPos, endPos, content))
  }

  def goToDef(
    driver: ServerDriver,
    workspace: os.Path,
    path: os.Path,
    pos: l.Position
  ): DefinitionResult = {
    val locations = driver.definition(path, pos)
    expect(locations.length >= 1)
    definitionResultOf(workspace, locations.head)
  }

  def goToDefs(
    driver: ServerDriver,
    workspace: os.Path,
    path: os.Path,
    pos: l.Position
  ): Seq[DefinitionResult] =
    driver.definition(path, pos).map(definitionResultOf(workspace, _))

  final case class DefinitionResult(
    path: String,
    line: Int,
    colRange: (Int, Int),
    content: String
  ) {
    def goToDefResult = GoToDefResult(
      Right(os.SubPath(path)),
      (line, colRange._1),
      (line, colRange._2),
      content
    )
    def colAverage: Int =
      (colRange._1 + colRange._2) / 2
  }
  object DefinitionResult {
    implicit lazy val codec: JsonValueCodec[DefinitionResult]         = JsonCodecMaker.make
    implicit lazy val seqCodec: JsonValueCodec[Seq[DefinitionResult]] = JsonCodecMaker.make
    def apply(goToDefResult: GoToDefResult): DefinitionResult = {
      if (goToDefResult.startPos._1 != goToDefResult.endPos._1)
        sys.error(s"Expected single line destination ($goToDefResult)")
      DefinitionResult(
        goToDefResult.defPath.left.map(path => sys.error(s"unexpected path $path")).map(
          _.toString
        ).merge,
        goToDefResult.startPos._1,
        (goToDefResult.startPos._2, goToDefResult.endPos._2),
        goToDefResult.content
      )
    }
  }

  case class CompletionItem(
    label: String,
    newText: String,
    editStart: (Int, Int),
    editEnd: (Int, Int),
    filterText: String,
    detail: String,
    additionalTextEdit: List[TextEdit] = Nil
  )

  case class TextEdit(
    editStart: (Int, Int),
    editEnd: (Int, Int),
    newText: String
  )

  def completions(
    driver: ServerDriver,
    path: os.Path,
    pos: l.Position
  ): Seq[CompletionItem] = {

    val itemsResp = driver.completion(path, pos)
    expect(itemsResp.getItemDefaults == null)

    itemsResp
      .getItems
      .asScala
      .toList
      .map { item =>
        val edit = item.getTextEdit.getLeft
        val additional = Option(item.getAdditionalTextEdits)
          .toList
          .flatMap(_.asScala.toList)
          .map { edit =>
            TextEdit(
              (edit.getRange.getStart.getLine, edit.getRange.getStart.getCharacter),
              (edit.getRange.getEnd.getLine, edit.getRange.getEnd.getCharacter),
              edit.getNewText
            )
          }
        CompletionItem(
          item.getLabel,
          edit.getNewText,
          (edit.getRange.getStart.getLine, edit.getRange.getStart.getCharacter),
          (edit.getRange.getEnd.getLine, edit.getRange.getEnd.getCharacter),
          item.getFilterText,
          item.getDetail,
          additionalTextEdit = additional
        )
      }
  }

  /** The raw completion response, in the shape it has on the wire.
    *
    * The server always answers with a completion list rather than a bare list of items, so this
    * wraps it back the way lsp4j does - what the fixtures were recorded from.
    */
  def completions0(
    driver: ServerDriver,
    path: os.Path,
    pos: l.Position
  ): l.jsonrpc.messages.Either[java.util.List[l.CompletionItem], l.CompletionList] =
    l.jsonrpc.messages.Either.forRight(driver.completion(path, pos))

  def scalaCliUrl(
    arch: String = sys.props.getOrElse("os.arch", "").toLowerCase(Locale.ROOT),
    version: String = IntegrationConstants.scalaCliVersion,
    isWin: Boolean = Properties.isWin,
    isMac: Boolean = Properties.isMac,
    isLinux: Boolean = Properties.isLinux
  ): Option[String] =
    arch match {
      case "x86_64" | "amd64" =>
        if (isWin)
          Some(
            s"https://github.com/VirtusLab/scala-cli/releases/download/v$version/scala-cli-x86_64-pc-win32.zip"
          )
        else if (isMac)
          Some(
            s"https://github.com/VirtusLab/scala-cli/releases/download/v$version/scala-cli-x86_64-apple-darwin.gz"
          )
        else if (isLinux)
          Some(
            s"https://github.com/VirtusLab/scala-cli/releases/download/v$version/scala-cli-x86_64-pc-linux.gz"
          )
        else None
      case "aarch64" =>
        if (isLinux)
          Some(
            s"https://github.com/VirtusLab/scala-cli/releases/download/v$version/scala-cli-aarch64-pc-linux.gz"
          )
        else if (isMac)
          Some(
            s"https://github.com/VirtusLab/scala-cli/releases/download/v$version/scala-cli-aarch64-apple-darwin.gz"
          )
        else None
      case _ =>
        None
    }

  lazy val scalaCli = {
    val url = scalaCliUrl().getOrElse {
      sys.error("No Scala CLI binary available for this platform")
    }
    val artifact = coursierapi.Artifact.of(url)
    val f        = os.Path(coursierapi.ArchiveCache.create().get(artifact), os.pwd)
    val f0 =
      if (Properties.isWin && os.isDir(f) && f.last.endsWith(".zip"))
        os.list(f)
          .find(_.last.endsWith(".exe"))
          .getOrElse(sys.error(s"No .exe found under $f"))
      else
        f
    if (!Properties.isWin && !f0.toIO.canExecute())
      os.perms.set(f0, "rwxr-xr-x")
    f0
  }

  // private lazy val isArm64 =
  //   Option(System.getProperty("os.arch")).map(_.toLowerCase(Locale.ROOT)).exists {
  //     case "aarch64" | "arm64" => true
  //     case _                   => false
  //   }
  lazy val jvmValues =
    Seq(
      // Labelled("8", if (isArm64 && Properties.isMac) "zulu:8.0.432" else "temurin:8.0-432"),
      Labelled("17", "temurin:17.0.7")
    )

  private def buildTools = Seq(
    SingleModuleBuildTool.ScalaCli(),
    SingleModuleBuildTool.Mill,
    SingleModuleBuildTool.Sbt
  )

  def buildToolJvmValues: Seq[(SingleModuleBuildTool, Labelled[String], String)] =
    for {
      buildTool <- buildTools
      jvm       <- jvmValues
      if jvm.label != "8" || buildTool != SingleModuleBuildTool.Mill // issue with Mill 0.11.7 and BSP, that requires Java >= 11
      testNameSuffix = s" ${buildTool.displayName} Java ${jvm.label}"
    } yield (buildTool, jvm, testNameSuffix)

  val scala213        = Labelled("2.13", IntegrationConstants.scala213)
  val scala213Compat  = Labelled(scala213.label + "-compat", scala213.value)
  val compatServerOpt = Seq("--scala2-compat=true")

  lazy val scalaVersionBuildToolJvmValues
    : Seq[(
      Option[Labelled[String]],
      Seq[String],
      SingleModuleBuildTool,
      Labelled[String],
      String
    )] = scalaVersionBuildToolJvmValues0(scripting = false)

  def scalaVersionBuildToolJvmValues0(scripting: Boolean)
    : Seq[(
      Option[Labelled[String]],
      Seq[String],
      SingleModuleBuildTool,
      Labelled[String],
      String
    )] =
    for {
      buildTool <- buildTools ++ {
        if (scripting) Seq(SingleModuleBuildTool.ScalaCli(scriptMode = true))
        else Nil
      }
      (scalaVersion, serverOpt) <- {
        val maybeScala213 =
          if (disableScala2Pc) Nil
          else Seq((scala213, Nil))
        maybeScala213 ++ Seq(
          (scala213Compat, compatServerOpt),
          (Labelled("3", IntegrationConstants.scala3), Nil)
        )
      }
      jvm <- jvmValues
      if jvm.label != "8" || buildTool != SingleModuleBuildTool.Mill // issue with Mill 0.11.7 and BSP, that requires Java >= 11
      testNameSuffix =
        s" ${buildTool.displayName} Scala ${scalaVersion.label} Java ${jvm.label}"
    } yield (Some(scalaVersion), serverOpt, buildTool, jvm, testNameSuffix)

  lazy val olderScalaVersionBuildToolJvmValues
    : Seq[(
      Option[Labelled[String]],
      Seq[String],
      SingleModuleBuildTool,
      Labelled[String],
      String
    )] =
    for {
      buildTool <- buildTools
      (scalaVersion, serverOpt) <- {
        val maybeScala213 =
          if (disableScala2Pc) Nil
          else Seq((Labelled("2.13.16", "2.13.16"), Nil))
        maybeScala213 ++ Seq(
          (Labelled("2.13.16-compat", "2.13.16"), compatServerOpt),
          (Labelled("3.7.4", "3.7.4"), Nil)
        )
      }
      jvm <- jvmValues
      if jvm.label != "8" || buildTool != SingleModuleBuildTool.Mill // issue with Mill 0.11.7 and BSP, that requires Java >= 11
      testNameSuffix =
        s" ${buildTool.displayName} Scala ${scalaVersion.label} Java ${jvm.label}"
    } yield (Some(scalaVersion), serverOpt, buildTool, jvm, testNameSuffix)

  private def same[T](got: T, expected: T): Boolean =
    (got, expected) match {
      case (gotArr: Array[Object], expectedArr: Array[Object]) =>
        Arrays.equals(gotArr, expectedArr)
      case _ =>
        got == expected
    }

  def checkFixture[T](
    path: os.Path,
    res: T,
    osOpt: Option[OutputStream],
    read: Array[Byte] => T,
    write: T => Array[Byte],
    roundTrip: Boolean = false
  ): Unit = {

    val res0 =
      if (roundTrip) read(write(res))
      else res
    val os0 = osOpt.getOrElse(System.err)
    if (TestParams.updateSnapshotsFast) {
      val expectedResOpt =
        if (os.exists(path))
          try Some(read(os.read.bytes(path)))
          catch {
            case e: JsonReaderException =>
              System.err.println(s"Warning: caught $e while reading $path")
              None
            case e: JsonSyntaxException =>
              System.err.println(s"Warning: caught $e while reading $path")
              None
          }
        else
          None
      expectedResOpt match {
        case None =>
          os0.write((s"Writing $path" + System.lineSeparator()).getBytes("UTF-8"))
          os0.flush()
          // over is for when parsing the file failed, see JsonSyntaxException above
          os.write.over(path, write(res), createFolders = true)
        case Some(expectedRes) =>
          if (!same(res0, expectedRes)) {
            os0.write((s"Updating $path" + System.lineSeparator()).getBytes("UTF-8"))
            os0.flush()
            os.write.over(path, write(res))
          }
      }
    }
    else {
      val expectedRes = read(os.read.bytes(path))
      val equals      = same(res0, expectedRes)
      if (!equals) {
        // TODO Print a diff here
        pprint.err.log(expectedRes)
        pprint.err.log(res0)
      }
      expect(equals)
    }
  }

  def checkJsoniterFixture[T: JsonValueCodec](
    path: os.Path,
    res: T,
    osOpt: Option[OutputStream]
  ): Unit =
    checkFixture[T](
      path,
      res,
      osOpt,
      b => readFromArray(b),
      writeToArray(_, WriterConfig.withIndentionStep(2))
    )

  def doReplaceAll(replaceAll: Seq[(String, String)])(
    input: String,
    inverse: Boolean = false
  ): String =
    replaceAll.foldLeft(input) {
      case (input0, (from0, to0)) =>
        val (from, to) = if (inverse) (to0, from0) else (from0, to0)
        input0.replace(from, to)
    }

  def checkGsonFixture[T: ClassTag](
    path: os.Path,
    res: T,
    osOpt: Option[OutputStream],
    replaceAll: Seq[(String, String)] = Nil,
    roundTrip: Boolean = false
  ): Unit =
    checkFixture[T](
      path,
      res,
      osOpt,
      bytes =>
        new Gson().fromJson[T](
          doReplaceAll(replaceAll)(new String(bytes, StandardCharsets.UTF_8), inverse = true),
          implicitly[ClassTag[T]].runtimeClass
        ),
      t => {
        val s = new GsonBuilder()
          .setPrettyPrinting()
          .create()
          .toJson(t, implicitly[ClassTag[T]].runtimeClass)
        doReplaceAll(replaceAll)(s).getBytes(StandardCharsets.UTF_8)
      },
      roundTrip = roundTrip
    )

  def checkTextFixture(
    path: os.Path,
    res: String,
    osOpt: Option[OutputStream]
  ): Unit =
    checkFixture[String](
      path,
      res,
      osOpt,
      new String(_, "UTF-8"),
      _.getBytes("UTF-8")
    )

  def standardReplacements(workspace: os.Path): Seq[(String, String)] =
    Seq(
      workspace.toNIO.toUri.toASCIIString                             -> "file:///workspace/",
      workspace.toIO.toURI.toASCIIString                              -> "file:/workspace/",
      s""""standalone:${IntegrationConstants.defaultScalaVersion}"""" -> """"standalone:_""""
    )

  def standardReplacementsExtra(workspace: os.Path): Seq[(String, String)] =
    Seq(
      workspace.toNIO.toUri.toASCIIString                             -> "file:///workspace/",
      workspace.toIO.toURI.toASCIIString                              -> "file:/workspace/",
      s""""standalone:${IntegrationConstants.defaultScalaVersion}"""" -> """"standalone:_"""",
      workspace.toString                                              -> "{workspace}",
      FileCache().location.toString                                   -> "{coursier_cache}"
    )

  def addGeneratedResources(
    workspace: os.Path,
    resourcesPath: os.SubPath,
    replaceAll: Seq[(String, String)],
    keep: os.SubPath => Boolean,
    isJson: os.SubPath => Boolean
  )(generate: => Unit): Unit = {
    val onDisk = generatedResourcesDir / resourcesPath
    if (TestParams.updateSnapshots) {
      generate

      val toUpdate = os.walk(workspace)
        .filter(os.isFile)
        .map(_.relativeTo(workspace).asSubPath)
        .filter(keep)
      val toRemove =
        if (os.exists(onDisk)) {
          val toUpdateSet = toUpdate.toSet
          os.walk(onDisk)
            .filter(os.isFile)
            .map(_.relativeTo(onDisk).asSubPath)
            .filter(!toUpdateSet.contains(_))
        }
        else
          Nil

      for (f <- toRemove)
        os.remove(onDisk / f)

      for (f <- toUpdate) {
        val content        = os.read(workspace / f)
        val updatedContent = doReplaceAll(replaceAll)(content)
        val updatedAndFormattedContent =
          if (isJson(f))
            ujson.write(ujson.read(updatedContent), indent = 2)
          else
            updatedContent
        val bytes = updatedAndFormattedContent.getBytes(StandardCharsets.UTF_8)
        val bytesOnDiskOpt =
          if (os.exists(onDisk / f)) Some(os.read.bytes(onDisk / f))
          else None
        if (bytesOnDiskOpt.forall(b => !Arrays.equals(b, bytes))) {
          System.err.println(s"Updating ${onDisk / f}")
          os.write.over(onDisk / f, updatedAndFormattedContent, createFolders = true)
        }
      }
    }
    else if (os.exists(onDisk))
      os.walk(onDisk)
        .filter(os.isFile)
        .map(_.relativeTo(onDisk).asSubPath)
        .foreach { f =>
          val content        = os.read(onDisk / f)
          val updatedContent = doReplaceAll(replaceAll)(content, inverse = true)
          System.err.println(s"Writing $f")
          os.write(workspace / f, updatedContent, createFolders = true)
        }
    else
      sys.error(s"$onDisk not found")
  }
}
