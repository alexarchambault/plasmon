package plasmon.integration

import com.eed3si9n.expecty.Expecty.expect
import com.github.plokhotnyuk.jsoniter_scala.core._
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import com.google.gson.{Gson, GsonBuilder}
import coursier.cache.FileCache
import io.github.alexarchambault.testutil.{OutputFrame, ProcessTest}
import io.github.alexarchambault.testutil.TestOutput.FixedReadBytes
import io.github.alexarchambault.testutil.TestUtil.*
import org.eclipse.lsp4j.jsonrpc.Launcher
import org.eclipse.lsp4j.services.LanguageServer
import org.eclipse.{lsp4j => l}

import java.io.OutputStream
import java.net.URI
import java.nio.charset.StandardCharsets
import java.nio.file.Paths
import java.util.Locale
import java.util.concurrent.{Future => JFuture, _}

import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.jdk.CollectionConverters.*
import scala.reflect.ClassTag
import scala.util.Properties
import java.util.Arrays
import scala.annotation.nowarn
import scala.concurrent.duration.Duration

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
    projectName: String = "test-project",
    client: l.services.LanguageClient = new MockLanguageClient {},
    clientCapabilities: l.ClientCapabilities = new l.ClientCapabilities,
    timeout: Option[FiniteDuration] = Some(baseTimeout),
    extraServerOpts: Seq[String] = Nil,
    count: Int = 1
  )(
    content: (os.SubPath, String)*
  )(f: (os.Path, LanguageServer, Positions, Option[OutputStream], Int) => T): T = {

    val pos = Positions.of(content: _*)
    val updatedContent = content.map {
      case (path, _) =>
        path -> (pos.content(path): os.Source)
    }

    withWorkspaceAndServer(
      projectName,
      client,
      clientCapabilities,
      timeout = timeout,
      extraServerOpts = extraServerOpts,
      count = count
    )(updatedContent: _*) {
      (workspace, remoteServer, _, osOpt, runCount) =>
        f(workspace, remoteServer, pos, osOpt, runCount)
    }
  }

  def withWorkspaceServerPositions[T](
    projectName: String = "test-project",
    client: l.services.LanguageClient = new MockLanguageClient {},
    clientCapabilities: l.ClientCapabilities = new l.ClientCapabilities,
    timeout: Option[FiniteDuration] = Some(baseTimeout),
    extraServerOpts: Seq[String] = Nil,
    count: Int = 1
  )(
    content: (os.SubPath, String)*
  )(f: (os.Path, LanguageServer, Positions, Option[OutputStream]) => T): T =
    withWorkspaceServerPositionsCount(
      projectName,
      client,
      clientCapabilities,
      timeout,
      extraServerOpts,
      count
    )(content: _*) {
      (workspace, remoteServer, pos, osOpt, _) =>
        f(workspace, remoteServer, pos, osOpt)
    }

  def serverExtraJavaOpts = Seq("-Duser.country=US", "-Duser.language=en")
  def serverEnv = Map(
    "PLASMON_JAVAC_EXTRA_OPTIONS" -> "-verbose"
  )
  val enableOutputFrame =
    // On Windows, OutputFrame stuff crashes if we don't have an actual terminal
    (!Properties.isWin || io.github.alexarchambault.isterminal.IsTerminal.isTerminal()) &&
    System.getenv("CI") == null
  def withWorkspaceAndServer[T](
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
  )(f: (os.Path, LanguageServer, JFuture[Void], Option[OutputStream], Int) => T): T = {

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
    ProcessTest(
      os.proc(
        baseCommand,
        "server",
        "--log-to-stderr",
        "--scala-cli",
        scalaCli,
        extraServerOpts
      ),
      timeout = timeout,
      count = count,
      env = serverEnv,
      runProcIn = tmpDir => {
        val dir = workspaceOpt.getOrElse(tmpDir / workingDir)
        os.makeDir.all(dir)
        dir
      },
      enableOutputFrame = enableOutputFrame,
      enableSilentOutput = TestParams.enableSilentOutput,
      printOutputOnError = TestParams.printOutputOnError,
      cleanUp = TestParams.cleanUpAfterTests,
      newOutputFrame = () => new OutputFrame(widthShift = -7)
    )(
      content.map { case (p, s) => (workingDir / p, s) }: _*
    ) {
      (tmpDir, subProc, ignoreSubProcExit, output, runCount) =>
        val workspace = workspaceOpt.getOrElse(tmpDir / workingDir)

        for (outputStream <- output.outputStreamOpt)
          client match {
            case client0: MockLanguageClient =>
              client0.setOutputStream(outputStream)
            case _ =>
          }

        val jsonrpcLauncher = new Launcher.Builder[LanguageServer]()
          .setExecutorService(pool)
          .setInput(subProc.stdout.wrapped)
          .setOutput(subProc.stdin.wrapped)
          .setRemoteInterface(classOf[LanguageServer])
          .setLocalService(client)
          .setExceptionHandler { t =>
            output.printStream.println(s"Error during LSP processing: $t")
            t.printStackTrace(output.printStream)
            output.printStream.flush()
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

        try
          f(workspace, remoteServer, listeningFuture, output.outputStreamOpt, runCount)
        finally
          if (shutdownServer) {
            output.printStream.println("Trying to ignore sub-process exit")
            output.printStream.flush()
            ignoreSubProcExit()
            output.printStream.println("Shutting down server")
            output.printStream.flush()
            remoteServer.shutdown().get(10L, TimeUnit.SECONDS)
            output.printStream.println("Exiting server")
            output.printStream.flush()
            remoteServer.exit()
          }
    }
  }

  private val baseCommand: os.Shellable =
    if (launcherKind == "native") Seq[os.Shellable](launcher, serverExtraJavaOpts)
    else Seq[os.Shellable]("java", serverExtraJavaOpts, "-jar", launcher)
  def runServerCommand(
    workspace: os.Path,
    err: Option[OutputStream]
  )(command: os.Shellable*): Unit = {
    val output = FixedReadBytes.pipeTo(err)
    os.proc(baseCommand, "command", "-v", command)
      .call(cwd = workspace, stdin = os.Inherit, stdout = output, mergeErrIntoOut = err.nonEmpty)
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
    err: Option[OutputStream]
  )(command: os.Shellable*): String = {
    val output = FixedReadBytes.pipeTo(err)
    val proc = os.proc(baseCommand, "command", "-v", command)
      .call(cwd = workspace, stderr = output)
    proc.out.text()
  }

  def identifier(path: os.Path): l.TextDocumentIdentifier =
    new l.TextDocumentIdentifier(path.toNIO.toUri.toASCIIString)

  def hoverMarkdown(
    remoteServer: LanguageServer,
    path: os.Path,
    pos: l.Position
  ): String = {

    val hoverResp = remoteServer
      .getTextDocumentService
      .hover(new l.HoverParams(identifier(path), pos))
      .get()

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

  def goToDef(
    remoteServer: LanguageServer,
    workspace: os.Path,
    path: os.Path,
    pos: l.Position
  ): DefinitionResult = {

    val defResp = remoteServer
      .getTextDocumentService
      .definition(new l.DefinitionParams(identifier(path), pos))
      .get()
    expect(defResp != null)
    expect(defResp.isLeft())
    val locations = defResp.getLeft.asScala.toList
    expect(locations.length >= 1)
    val location = locations.head

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
    implicit lazy val codec: JsonValueCodec[DefinitionResult] = JsonCodecMaker.make
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
    remoteServer: LanguageServer,
    path: os.Path,
    pos: l.Position
  ): Seq[CompletionItem] = {

    val completionResp = remoteServer
      .getTextDocumentService
      .completion(
        new l.CompletionParams(
          identifier(path),
          pos,
          new l.CompletionContext(l.CompletionTriggerKind.Invoked)
        )
      )
      .get()

    expect(completionResp != null)
    expect(completionResp.isRight())

    val itemsResp = completionResp.getRight
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

  def completions0(
    remoteServer: LanguageServer,
    path: os.Path,
    pos: l.Position
  ) =
    remoteServer
      .getTextDocumentService
      .completion(
        new l.CompletionParams(
          identifier(path),
          pos,
          new l.CompletionContext(l.CompletionTriggerKind.Invoked)
        )
      )
      .get()

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

  private lazy val buildTools = Seq(
    SingleModuleBuildTool.ScalaCli,
    SingleModuleBuildTool.Mill,
    SingleModuleBuildTool.Sbt
  )

  lazy val buildToolJvmValues: Seq[(SingleModuleBuildTool, Labelled[String], String)] =
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
    )] =
    for {
      buildTool <- buildTools
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

  lazy val projectsDir = {
    val path = sys.props.getOrElse(
      "plasmon.integration.projects",
      sys.error("plasmon.integration.projects not set")
    )
    os.Path(path, os.pwd)
  }

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
    alternativePaths: Seq[os.Path] = Nil,
    roundTrip: Boolean = false
  ): Unit = {

    val res0 =
      if (roundTrip) read(write(res))
      else res
    val os0 = osOpt.getOrElse(System.err)
    if (TestParams.updateSnapshotsFast) {
      val expectedResOpt = if (os.exists(path)) Some(read(os.read.bytes(path))) else None
      expectedResOpt match {
        case None =>
          os0.write((s"Writing $path" + System.lineSeparator()).getBytes("UTF-8"))
          os0.flush()
          os.write(path, write(res), createFolders = true)
          for (alt <- alternativePaths)
            os.remove(alt)
        case Some(expectedRes) =>
          if (!same(res0, expectedRes))
            if (alternativePaths.isEmpty) {
              os0.write((s"Updating $path" + System.lineSeparator()).getBytes("UTF-8"))
              os0.flush()
              os.write.over(path, write(res))
            }
            else {
              val possibleExpectedRes = alternativePaths
                .takeWhile(os.exists(_))
                .map(p => read(os.read.bytes(p)))
              val isInAlternatives = possibleExpectedRes.exists(same(res0, _))
              if (!isInAlternatives) {
                val nextPathOpt = alternativePaths.drop(possibleExpectedRes.length).headOption
                nextPathOpt match {
                  case Some(nextPath) =>
                    os0.write((s"Writing $nextPath" + System.lineSeparator()).getBytes("UTF-8"))
                    os0.flush()
                    os.write.over(nextPath, write(res))
                  case None =>
                    sys.error(
                      s"Cannot write alternative result for $path (needs more than ${alternativePaths.length} alternative paths)"
                    )
                }
              }
            }
      }
    }
    else if (TestParams.updateAlternativeSnapshots && alternativePaths.nonEmpty) {
      val possibleExpectedRes = (path +: alternativePaths)
        .takeWhile(os.exists(_))
        .map(p => read(os.read.bytes(p)))

      if (!possibleExpectedRes.exists(same(res0, _))) {
        val nextPathOpt = (path +: alternativePaths).drop(possibleExpectedRes.length).headOption
        nextPathOpt match {
          case Some(nextPath) =>
            os0.write((s"Writing $nextPath" + System.lineSeparator()).getBytes("UTF-8"))
            os0.flush()
            os.write.over(nextPath, write(res))
          case None =>
            sys.error(
              s"Cannot write alternative result for $path (needs more than ${alternativePaths.length} alternative paths)"
            )
        }
      }
    }
    else if (alternativePaths.nonEmpty) {
      val possibleExpectedRes = (path +: alternativePaths)
        .takeWhile(os.exists(_))
        .map(p => read(os.read.bytes(p)))
      expect(possibleExpectedRes.exists(same(res0, _)))
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
    osOpt: Option[OutputStream],
    alternativePaths: Seq[os.Path] = Nil
  ): Unit =
    checkFixture[T](
      path,
      res,
      osOpt,
      b =>
        try readFromArray(b)
        catch {
          case e: JsonReaderException =>
            throw new Exception(e)
        },
      writeToArray(_, WriterConfig.withIndentionStep(2)),
      alternativePaths = alternativePaths
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
    alternativePaths: Seq[os.Path] = Nil,
    roundTrip: Boolean = false
  ): Unit = {

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
      alternativePaths = alternativePaths,
      roundTrip = roundTrip
    )
  }

  def checkTextFixture(
    path: os.Path,
    res: String,
    osOpt: Option[OutputStream],
    alternativePaths: Seq[os.Path] = Nil
  ): Unit =
    checkFixture[String](
      path,
      res,
      osOpt,
      new String(_, "UTF-8"),
      _.getBytes("UTF-8"),
      alternativePaths = alternativePaths
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
