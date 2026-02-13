package plasmon.servercommand

import bloop.rifle.{BloopRifleConfig, BloopRifleLogger, BloopServer, BloopThreads, BloopVersion}
import ch.epfl.scala.{bsp4j => b}
import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import com.google.gson.Gson
import coursier.version.Version
import org.eclipse.lsp4j.jsonrpc.Launcher
import org.eclipse.{lsp4j => l}
import plasmon.internal.Directories
import plasmon.bsp.{BuildServerInfo, BuildServerLauncher, BuildServerProcess, BuildTool}
import plasmon.Logger
import java.util.{List => JList}
import plasmon.internal.Constants

import java.io.{File, InputStream, OutputStream}
import java.net.URI
import java.util.concurrent.ExecutorService

import scala.jdk.CollectionConverters.*
import plasmon.bsp.PlasmonBuildServer
import scala.util.Properties
import java.io.ByteArrayOutputStream
import java.io.PrintStream
import java.nio.charset.StandardCharsets
import plasmon.internal.DebugInput
import bloop.rifle.bloop4j.BloopExtraBuildParams
import plasmon.bsp.PlasmonBuildClient
import plasmon.bsp.LoggingPlasmonBuildServer
import bloop.rifle.BloopRifle
import plasmon.bsp.BspConnection
import plasmon.index.TargetData
import plasmon.PlasmonEnrichments._
import plasmon.bsp.PlasmonBuildClientImpl
import plasmon.languageclient.PlasmonLanguageClient
import java.util.UUID
import java.io.PrintWriter
import plasmon.index.BspData

object BspUtil {

  final case class TargetTag(tag: String)
  object TargetTag {
    def scala(version: String): Set[TargetTag] = {
      val suffixIdx = version.indexWhere(c => !c.isDigit && c != '.')
      val withSuffixTags =
        if (suffixIdx >= 0) Set(TargetTag(version))
        else Set.empty[TargetTag]
      val baseVersion =
        if (suffixIdx >= 0) version.take(suffixIdx)
        else version
      val baseTags = baseVersion
        .split('.')
        .inits
        .filter(_.nonEmpty)
        .map("scala-" + _.mkString("."))
        .map(TargetTag(_))
        .toSet
      Set(scalaProject) ++ baseTags ++ withSuffixTags
    }
    val scalaProject = TargetTag("scala")
    val java         = TargetTag("java")

    val scalaJs     = TargetTag("scala-js")
    val scalaNative = TargetTag("scala-native")
    val jvm         = TargetTag("scala-jvm")

    val sbt = TargetTag("sbt")

    val test        = TargetTag("test")
    val application = TargetTag("application")
    val library     = TargetTag("library")
  }

  def tags(target: b.BuildTarget): Set[TargetTag] = {
    val buildToolTags = target.getTags.asScala.iterator.map(TargetTag(_)).toSet
    val scalaOrJavaTags = target.asScalaBuildTarget.fold(Set(TargetTag.java)) { scalaTarget =>
      val platformTag = scalaTarget.getPlatform match {
        case b.ScalaPlatform.JVM    => TargetTag.jvm
        case b.ScalaPlatform.JS     => TargetTag.scalaJs
        case b.ScalaPlatform.NATIVE => TargetTag.scalaNative
      }
      TargetTag.scala(scalaTarget.getScalaVersion) + platformTag
    }
    val sbtTags = target.asSbtBuildTarget.fold(Set.empty[TargetTag]) { _ =>
      Set(TargetTag.sbt)
    }
    buildToolTags ++ scalaOrJavaTags ++ sbtTags
  }

  def tags(targets: Seq[b.BuildTarget]): Map[b.BuildTargetIdentifier, Set[TargetTag]] =
    targets
      .map(target => target.getId -> tags(target))
      .toMap

  def bspFile(workspace: os.Path): os.Path = {

    val bspDir = workspace / ".bsp"
    if (!os.exists(bspDir))
      sys.error(s"$bspDir not found")
    if (!os.isDir(bspDir))
      sys.error(s"$bspDir not a directory")
    val bspFiles = os.list(bspDir)
      .filter(_.last.endsWith(".json"))
      .filter(os.isFile)

    bspFiles match {
      case Seq() =>
        sys.error(s"No BSP file found under $bspDir")
      case Seq(f) => f
      case _ =>
        sys.error("Cannot import several BSP projects for now")
    }
  }

  def bloopConfig(): BloopRifleConfig = {
    val scalaCliBloopWorkingDir = {
      val dir = new Directories("ScalaCli")
      val baseDir =
        if (Properties.isMac) dir.cacheDir()
        else dir.dataLocalDir()
      os.Path(baseDir, os.pwd) / "bloop"
    }
    val scalaCliBloopDaemonDir = scalaCliBloopWorkingDir / "daemon"
    BloopRifleConfig.default(
      BloopRifleConfig.Address.DomainSocket(scalaCliBloopDaemonDir.toNIO),
      version =>
        try {
          val bloopModule = coursierapi.Module.parse(
            BloopRifleConfig.defaultModule,
            coursierapi.ScalaVersion.of(Properties.versionNumberString)
          )
          val cp = coursierapi.Fetch.create()
            .addDependencies(
              coursierapi.Dependency.of(bloopModule, version)
            )
            .fetch()
            .asScala
            .toVector
          Right((cp, true))
        }
        catch {
          case e: coursierapi.error.CoursierError =>
            Left(e)
        },
      scalaCliBloopWorkingDir.toIO
    ).copy(
      retainedBloopVersion =
        BloopRifleConfig.AtLeast(BloopVersion(Constants.bloopVersion))
    )
  }

  def bloopConfig(bloopJavaHome: () => os.Path): BloopRifleConfig =
    bloopConfig().copy(
      javaPath = {
        val home = bloopJavaHome()
        val ext  = if (Properties.isWin) ".exe" else ""
        val path = home / "bin" / s"java$ext"
        scribe.info(s"Bloop javaPath: " + pprint.apply(path))
        path.toString
      }
    )

  def bloopLogger(logger: Logger): BloopRifleLogger =
    new BloopRifleLogger {
      def info(msg: => String) =
        logger.log("[info] " + msg)
      def debug(msg: => String, ex: Throwable) =
        scribe.debug("Bloop: " + msg)
      def error(msg: => String, ex: Throwable) = {
        logger.log("[error] " + msg)
        val baos = new ByteArrayOutputStream
        ex.printStackTrace(new PrintStream(baos, true, StandardCharsets.UTF_8))
        logger.log(new String(baos.toByteArray, StandardCharsets.UTF_8))
      }
      def error(msg: => String) =
        logger.log("[error] " + msg)
      lazy val bloopBspStdout: Option[OutputStream] = Some(
        io.github.alexarchambault.testutil.OutputFrame.lineProcessorOutputStream { line =>
          logger.log(s"BSP: " + line)
        }
      )
      lazy val bloopBspStderr: Option[OutputStream] = Some(
        io.github.alexarchambault.testutil.OutputFrame.lineProcessorOutputStream { line =>
          logger.log(s"BSP: " + line)
        }
      )
      def bloopCliInheritStdout = false
      def bloopCliInheritStderr = false
    }

  def bloopExit(logger: Logger): Boolean = {
    val config       = bloopConfig()
    val bloopLogger0 = bloopLogger(logger)
    !BloopRifle.check(config, bloopLogger0) || {
      val retCode = BloopRifle.exit(config, os.pwd.toNIO, bloopLogger0)
      if (retCode != 0)
        scribe.warn(s"Could not shutdown Bloop server (exit code: $retCode)")
      retCode == 0
    }
  }

  def bspServerFromInfo(
    launcher: BuildServerLauncher,
    log: String => Unit,
    buildClient: () => PlasmonBuildClientImpl,
    languageClient: PlasmonLanguageClient,
    buildToolId: String,
    buildToolName: String,
    bspPool: ExecutorService,
    bloopThreads: () => BloopThreads,
    javaHome: os.Path,
    bloopJavaHome: () => os.Path,
    logger: Logger,
    outputLogger: Logger,
    logJsonrpcInput: Boolean,
    enableBestEffortMode: Boolean
  ): BspConnection = {

    launcher.prepare.foreach(_(logger, false))
    launcher.info.prepare.foreach(_(logger, false))

    launcher.info match {
      case b: BuildServerInfo.Bsp =>
        val bspFile = b.bspFile.map(b.workspace / _).merge
        val content =
          try readFromArray(os.read.bytes(bspFile))(using BspFile.codec)
          catch {
            case e: JsonReaderException =>
              throw new Exception(e)
          }
        val (proc0, buildServer, initRes, remoteEndpoint, buildClient0) = bspServerFromCommand(
          content.argv,
          b.workspace,
          log,
          buildClient,
          languageClient,
          buildToolId,
          buildToolName,
          bspPool,
          extraEnv = Map.empty,
          logger,
          outputLogger,
          logJsonrpcInput,
          enableBestEffortMode
        )

        BspConnection(initRes, launcher, buildServer, proc0, remoteEndpoint, buildClient0, logger)

      case b: BuildServerInfo.Bloop =>
        val config       = bloopConfig(bloopJavaHome)
        val bloopLogger0 = bloopLogger(logger)
        val (conn, socket, bloopInfo) = BloopServer.bsp(
          config,
          b.workspace.toNIO,
          bloopThreads(),
          bloopLogger0,
          config.period,
          config.timeout
        )
        scribe.info(s"bloopInfo: " + pprint.apply(bloopInfo))
        val (server, initRes, remoteEndpoint, buildClient0) = bspServerFromStreams(
          socket.getInputStream,
          socket.getOutputStream,
          b.workspace,
          log,
          buildClient,
          languageClient,
          buildToolId,
          buildToolName,
          bspPool,
          logJsonrpcInput, {
            val bloopExtraParams = new BloopExtraBuildParams
            // bloopExtraParams.setClientClassesRootDir((b.workspace / "foo").toNIO.toUri.toASCIIString)
            // bloopExtraParams.setOwnsBuildFiles(true)
            bloopExtraParams.setSemanticdbVersion(Constants.scalametaVersion)
            bloopExtraParams.setSupportedScalaVersions(
              List(Constants.scala2Version).asJava
            )

            BspExtraBuildParams(
              javaSemanticdbVersion = Constants.semanticdbJavaVersion,
              semanticdbVersion = Constants.scalametaVersion,
              supportedScalaVersions = Seq(Constants.scala2Version).asJava,
              enableBestEffortMode = enableBestEffortMode
            )
          },
          fakeMetals = true,
          logger
        )
        buildClient0.setLogger(logger)

        BspConnection(
          initRes,
          launcher,
          server,
          BuildServerProcess.BloopConnection(conn),
          remoteEndpoint,
          buildClient0,
          logger
        )

      case m: BuildServerInfo.Mill =>
        val command =
          Seq(m.commandName, "--bsp", "--disable-ticker", "--color", "false", "--jobs", "1")

        val (proc0, buildServer, initRes, remoteEndpoint, buildClient0) = bspServerFromCommand(
          command,
          m.workspace,
          log,
          buildClient,
          languageClient,
          buildToolId,
          buildToolName,
          bspPool,
          extraEnv = Map(
            "JAVA_HOME" -> javaHome.toString
          ),
          logger,
          outputLogger,
          logJsonrpcInput,
          enableBestEffortMode
        )

        BspConnection(initRes, launcher, buildServer, proc0, remoteEndpoint, buildClient0, logger)

      case m: BuildServerInfo.Sbt =>
        val bspFile = m.workspace / ".bsp/sbt.json"
        val content =
          try readFromArray(os.read.bytes(bspFile))(using BspFile.codec)
          catch {
            case e: JsonReaderException =>
              throw new Exception(e)
          }

        val command = content.argv

        scribe.info("javaHome=" + pprint.apply(javaHome))
        val (proc0, buildServer, initRes, remoteEndpoint, buildClient0) = bspServerFromCommand(
          command,
          m.workspace,
          log,
          buildClient,
          languageClient,
          buildToolId,
          buildToolName,
          bspPool,
          extraEnv = Map(
            "JAVA_HOME" -> javaHome.toString
          ),
          logger,
          outputLogger,
          logJsonrpcInput,
          enableBestEffortMode
        )

        BspConnection(initRes, launcher, buildServer, proc0, remoteEndpoint, buildClient0, logger)

      case s: BuildServerInfo.ScalaCli =>
        // Fetch scala-cli ourselves if it's missing?
        // TODO Better check for scala-cli extension on Windows by looking at PATH and PATHEXT
        val sources =
          if (s.paths.isEmpty) Seq(s.workspace.toString)
          else s.paths.map(_.toString)
        val command = s.scalaCliCommand ++ Seq("bsp", "--") ++ sources
        val (proc0, buildServer, initRes, remoteEndpoint, buildClient0) = bspServerFromCommand(
          command,
          s.workspace,
          log,
          buildClient,
          languageClient,
          buildToolId,
          buildToolName,
          bspPool,
          extraEnv = Map.empty,
          logger,
          outputLogger,
          logJsonrpcInput,
          enableBestEffortMode
        )

        BspConnection(initRes, launcher, buildServer, proc0, remoteEndpoint, buildClient0, logger)
    }
  }

  def bspServerFromBspFile(
    bspFile: os.Path,
    workspace: os.Path,
    log: String => Unit,
    buildClient: () => PlasmonBuildClientImpl,
    languageClient: PlasmonLanguageClient,
    buildToolId: String,
    buildToolName: String,
    bspPool: ExecutorService,
    logger: Logger,
    outputLogger: Logger,
    logJsonrpcInput: Boolean,
    enableBestEffortMode: Boolean
  ): (
    BuildServerProcess.Process,
    PlasmonBuildServer,
    b.InitializeBuildResult,
    l.jsonrpc.RemoteEndpoint,
    PlasmonBuildClient
  ) = {
    val content =
      try readFromArray(os.read.bytes(bspFile))(using BspFile.codec)
      catch {
        case e: JsonReaderException =>
          throw new Exception(e)
      }
    bspServerFromCommand(
      content.argv,
      workspace,
      log,
      buildClient,
      languageClient,
      buildToolId,
      buildToolName,
      bspPool,
      extraEnv = ???,
      logger,
      outputLogger,
      logJsonrpcInput,
      enableBestEffortMode
    )
  }

  def bspServerFromCommand(
    command: Seq[String],
    workspace: os.Path,
    log: String => Unit,
    buildClient: () => PlasmonBuildClientImpl,
    languageClient: PlasmonLanguageClient,
    buildToolId: String,
    buildToolName: String,
    bspPool: ExecutorService,
    extraEnv: Map[String, String],
    logger: Logger,
    outputLogger: Logger,
    logJsonrpcInput: Boolean,
    enableBestEffortMode: Boolean
  ): (
    BuildServerProcess.Process,
    PlasmonBuildServer,
    b.InitializeBuildResult,
    l.jsonrpc.RemoteEndpoint,
    PlasmonBuildClientImpl
  ) = {

    scribe.info("command=" + pprint.apply(command))

    val proc = outputLogger.logCommand(os.proc(command)).spawn(
      cwd = workspace,
      stderr = outputLogger.processOutput,
      env = extraEnv
    )
    val proc0 = BuildServerProcess.Process(
      proc,
      BuildServerProcess.Process.createWatchThread(proc, outputLogger)
    )

    // FIXME Watch if proc exits

    val (buildServer, initRes, remoteEndpoint, buildClient0) = bspServerFromStreams(
      proc.stdout.wrapped,
      proc.stdin.wrapped,
      workspace,
      log,
      buildClient,
      languageClient,
      buildToolId,
      buildToolName,
      bspPool,
      logJsonrpcInput,
      // for Mill?
      BspExtraBuildParams(
        javaSemanticdbVersion = Constants.semanticdbJavaVersion,
        semanticdbVersion = Constants.scalametaVersion,
        supportedScalaVersions = Seq(Constants.scala2Version).asJava,
        enableBestEffortMode = enableBestEffortMode
      ),
      fakeMetals = true,
      logger
    )
    buildClient0.setLogger(logger)

    (proc0, buildServer, initRes, remoteEndpoint, buildClient0)
  }

  def bspServerFromStreams(
    in: InputStream,
    out: OutputStream,
    workspace: os.Path,
    log: String => Unit,
    buildClient: () => PlasmonBuildClientImpl,
    languageClient: PlasmonLanguageClient,
    buildToolId: String,
    buildToolName: String,
    bspPool: ExecutorService,
    logJsonrpcInput: Boolean,
    initData: Object,
    fakeMetals: Boolean,
    logger: Logger
  ): (
    PlasmonBuildServer,
    b.InitializeBuildResult,
    l.jsonrpc.RemoteEndpoint,
    PlasmonBuildClientImpl
  ) = {

    val (input, output) =
      if (logJsonrpcInput)
        DebugInput.debug(
          in,
          out,
          (line, isOut) => scribe.info(s"Build client ${if (isOut) ">" else "<"} $line")
        )
      else
        (in, out)

    val buildClient0 = buildClient()

    val launcher = new Launcher.Builder[PlasmonBuildServer]()
      .setExecutorService(bspPool)
      .setInput(input)
      .setOutput(output)
      .setRemoteInterface(classOf[PlasmonBuildServer])
      .setLocalService(buildClient0)
      .setExceptionHandler { t =>
        scribe.info("Error during build server call", t)
        l.jsonrpc.RemoteEndpoint.DEFAULT_EXCEPTION_HANDLER.apply(t)
      }
      // .traceMessages(
      //   new PrintWriter((workspace / "bsp.log").toIO)
      // )
      // .wrapMessages { c =>
      //   new l.jsonrpc.MessageConsumer {
      //     def consume(msg: l.jsonrpc.messages.Message): Unit = {
      //       scribe.info("Got message " + pprint.apply(msg))
      //       c.consume(msg)
      //     }
      //   }
      // }
      .create()

    val buildServer = new LoggingPlasmonBuildServer(
      launcher.getRemoteProxy,
      logger,
      languageClient,
      buildToolId,
      buildToolName
    )

    log("Connecting to build server")
    languageClient.reportProgress(buildToolId, buildToolName, "Connecting to build server") {
      launcher.startListening()
    }
    log("Connected to build server")

    val initRes =
      languageClient.reportProgress(buildToolId, buildToolName, "Initializing build server") {
        buildServer
          .buildInitialize {
            val params = new b.InitializeBuildParams(
              if (fakeMetals) "Metals" else "Plasmon",
              "0.1.0-SNAPSHOT",
              ch.epfl.scala.bsp4j.Bsp4j.PROTOCOL_VERSION,
              workspace.toNIO.toUri.toASCIIString,
              new b.BuildClientCapabilities(List("scala", "java").asJava)
            )
            params.setData(new Gson().toJsonTree(initData))
            params
          }
          .get()
      }

    buildServer.onBuildInitialized()

    scribe.info(s"BSP server initialization result: $initRes")

    buildClient0.setBuildToolName(BspConnection.enhancedName(initRes.getDisplayName))

    (buildServer, initRes, launcher.getRemoteEndpoint, buildClient0)
  }

  def targetShortId(bspData: BspData, target: b.BuildTargetIdentifier): String =
    bspData.info(target).map(_.getDisplayName).getOrElse(target.getUri)

  def targetFullId(projectPath: os.Path, maybeShortId: String): b.BuildTargetIdentifier =
    if (maybeShortId.contains(":/"))
      new b.BuildTargetIdentifier(maybeShortId)
    else {
      val queryStartIdx = maybeShortId.indexOf('?')
      val (pathPart, queryOpt) =
        if (queryStartIdx < 0) (maybeShortId, None)
        else (maybeShortId.take(queryStartIdx), Some(maybeShortId.drop(queryStartIdx + 1)))
      val fullPath = projectPath / os.SubPath(pathPart)
      val uri      = fullPath.toNIO.toUri.toASCIIString + queryOpt.fold("")("?" + _)
      new b.BuildTargetIdentifier(uri)
    }

  def addTransitiveDependencies[T](
    roots: Seq[b.BuildTargetIdentifier],
    allTargets: Seq[b.BuildTarget]
  ): Set[b.BuildTargetIdentifier] =
    addTransitiveDependencies(
      roots.toSet,
      allTargets.map(t => t.getId -> t.getDependencies.asScala.toSet).toMap
    )

  def addTransitiveDependencies[T](rootDeps: Set[T], depMap: Map[T, Set[T]]): Set[T] = {
    val extraTransitiveDeps = rootDeps.flatMap(dep => depMap.getOrElse(dep, Set.empty[T]))
    if (extraTransitiveDeps.exists(dep => !rootDeps.contains(dep)))
      addTransitiveDependencies(rootDeps ++ extraTransitiveDeps, depMap -- rootDeps)
    else
      rootDeps
  }

  def targetsByConnection(
    allTargetData: Seq[TargetData],
    log: String => Unit,
    path: os.Path,
    options: SharedBspOptions
  ) = {

    val constraints = options.require
      .map(_.trim)
      .filter(_.nonEmpty)
      .flatMap(TagConstraint.parse)

    def allTargetConnections0 = allTargetData.toVector.flatMap { data =>
      for {
        buildServer <- data.buildServerOpt.toSeq
        (id, _)     <- data.targetToWorkspace
      } yield (id, buildServer, data)
    }
    def allTargetConnections = allTargetData.toVector.flatMap { data =>
      data.buildServerOpt.iterator.flatMap { server =>
        data.targetToWorkspace.iterator.map(_._1)
          .map(id => (id, data.info(id).map(_.getDisplayName).getOrElse(id.getUri), server))
      }
    }

    val targetConnections =
      if (constraints.isEmpty)
        allTargetConnections
      else
        allTargetConnections0
          .flatMap {
            case (id, server, data) =>
              val target = data
                .buildTargetInfo
                .getOrElse(id, sys.error(s"No BuildTarget data found for $id"))
              val tags = BspUtil.tags(target)
              if (constraints.forall(_.validate(tags)))
                Seq((id, target.getDisplayName, server))
              else
                Nil
          }

    targetConnections
      .groupBy(_._3)
      .map {
        case (conn, list) =>
          (conn, list.map(_._1).sortBy(_.getUri))
      }
      .toVector
    // FIXME Sort that somehow
  }

  final case class BspFile(
    name: String,
    argv: Seq[String]
  )
  object BspFile {
    val codec: JsonValueCodec[BspFile] = JsonCodecMaker.make
  }

  final case class TagConstraint(require: Seq[Either[TargetTag, TargetTag]]) {
    def validate(tags: Set[TargetTag]): Boolean =
      require.exists {
        case Left(forbiddenTag) => !tags.contains(forbiddenTag)
        case Right(requiredTag) => tags.contains(requiredTag)
      }
  }
  object TagConstraint {
    def parse(input: String): Seq[TagConstraint] =
      input.split(',').toSeq.map { input0 =>
        val req = input0.split('|')
          .map { tag =>
            if (tag.startsWith("!"))
              Left(TargetTag(tag.stripPrefix("!")))
            else
              Right(TargetTag(tag))
          }
          .toSeq
        TagConstraint(req)
      }
  }

  final case class DiscoveredBuildTool(
    discoverId: String,
    buildTool: BuildTool,
    warning: Option[String]
  )

  trait BuildToolDiscover {
    def ids: Seq[String]
    def check(
      workspace: os.Path,
      currentFile: Option[os.Path],
      alreadyAdded: Set[plasmon.bsp.BuildTool]
    ): Seq[DiscoveredBuildTool]
  }

  object BuildToolDiscover {
    def all = Seq[BuildToolDiscover](
      Mill,
      Sbt,
      Bloop,
      ScalaCli
    )

    lazy val map: Map[String, BuildToolDiscover] =
      all.iterator.flatMap(d => d.ids.iterator.map(_ -> d)).toMap

    case object Mill extends BuildToolDiscover {
      def ids = Seq(BuildTool.Mill.id, BuildTool.MillViaBloop.id)
      def check(
        workspace: os.Path,
        currentFile: Option[os.Path],
        alreadyAdded: Set[plasmon.bsp.BuildTool]
      ) = {

        val buildSc            = workspace / "build.sc"
        val buildMill          = workspace / "build.mill"
        val altBuildMill       = workspace / "build.mill.scala"
        val millScriptPath     = workspace / "mill"
        val millVersionPath    = workspace / ".mill-version"
        val millVersionAltPath = workspace / ".config/mill-version"

        val hasMillBuildFile = os.isFile(buildSc) || os.isFile(buildMill) || os.isFile(altBuildMill)
        if (hasMillBuildFile && os.isFile(millScriptPath)) {
          def versionFromMillBuild(path: os.Path): Option[Version] =
            os.read.lines(path)
              .map(_.trim)
              .filter(_.nonEmpty)
              .takeWhile(_.startsWith("//| "))
              .map(_.stripPrefix("//| "))
              .find(_.startsWith("mill-version: "))
              .map(_.stripPrefix("mill-version: "))
              .map(Version(_))

          val millVersionOpt =
            if (os.isFile(millVersionAltPath)) Some(Version(os.read(millVersionAltPath).trim()))
            else if (os.isFile(millVersionPath)) Some(Version(os.read(millVersionPath).trim()))
            else if (os.exists(buildMill)) versionFromMillBuild(buildMill)
            else if (os.exists(altBuildMill)) versionFromMillBuild(altBuildMill)
            else None
          val minMillBspVersion = Version("0.11")
          val warningOpt = millVersionOpt.map(ver => (ver, ver >= minMillBspVersion)) match {
            case Some((_, true)) => None
            case Some((ver, false)) =>
              Some(s"Warning: Mill ${ver.repr} not guaranteed to work fine from Plasmon")
            case None => Some(
                "Warning: no Mill version found, Mill build not guaranteed to work fine from Plasmon"
              )
          }
          val maybeMillBloop =
            if (millVersionOpt.exists(_.repr.startsWith("0.")))
              Seq(
                DiscoveredBuildTool(
                  BuildTool.MillViaBloop.id,
                  BuildTool.MillViaBloop(workspace),
                  warningOpt
                )
              )
            else
              Nil
          Seq(DiscoveredBuildTool(BuildTool.Mill.id, BuildTool.Mill(workspace), warningOpt)) ++
            maybeMillBloop
        }
        else
          Nil
      }
    }

    case object Sbt extends BuildToolDiscover {
      def ids = Seq(BuildTool.Sbt.id, BuildTool.SbtViaBloop.id)
      def check(
        workspace: os.Path,
        currentFile: Option[os.Path],
        alreadyAdded: Set[plasmon.bsp.BuildTool]
      ) = {

        val buildSbt        = workspace / "build.sbt"
        val buildProperties = workspace / "project/build.properties"

        if (os.isFile(buildSbt) && os.isFile(buildProperties))
          Seq(
            DiscoveredBuildTool(BuildTool.Sbt.id, BuildTool.Sbt(workspace), None),
            DiscoveredBuildTool(BuildTool.SbtViaBloop.id, BuildTool.SbtViaBloop(workspace), None)
          )
        else
          Nil
      }
    }

    case object Bloop extends BuildToolDiscover {
      def ids = Seq(BuildTool.Bloop.id)
      def check(
        workspace: os.Path,
        currentFile: Option[os.Path],
        alreadyAdded: Set[plasmon.bsp.BuildTool]
      ): Seq[DiscoveredBuildTool] = {
        val dotBloop = workspace / ".bloop"
        if (os.isDir(dotBloop))
          Seq(
            DiscoveredBuildTool(BuildTool.Bloop.id, BuildTool.Bloop(workspace), None)
          )
        else
          Nil
      }
    }

    case object ScalaCli extends BuildToolDiscover {
      def ids = Seq(BuildTool.ScalaCli.id)
      def check(
        workspace: os.Path,
        currentFile: Option[os.Path],
        alreadyAdded: Set[plasmon.bsp.BuildTool]
      ): Seq[DiscoveredBuildTool] = {

        val alreadyAdded0 = alreadyAdded
          .collect {
            case sc: BuildTool.ScalaCli =>
              if (sc.sources.isEmpty) Seq(sc.workspace)
              else sc.sources
          }
          .flatten
          .toVector
          .sortBy(_.segmentCount)

        val forCurrentFile0 = currentFile.toSeq.flatMap { f =>
          val forDir  = if (isScalaCliDir(f)) Seq(f) else Nil
          val forFile = if (isScalaCliFile(f)) Seq(f, f / os.up) else Nil
          forDir ++ forFile
        }

        val forWorkspace0 = if (isScalaCliDir(workspace)) Seq(workspace) else Nil

        val elems = (forCurrentFile0 ++ forWorkspace0).distinct.filter { elem =>
          !alreadyAdded0.exists(elem.startsWith)
        }

        elems.map { elem =>
          DiscoveredBuildTool(
            BuildTool.ScalaCli.id,
            if (os.isFile(elem)) BuildTool.ScalaCli(workspace, Seq(elem))
            else BuildTool.ScalaCli(elem, Nil),
            None
          )
        }
      }

      private def isScalaCliDirOrFile(path: os.Path): Boolean =
        isScalaCliDir(path) || isScalaCliFile(path)

      private def isScalaCliDir(path: os.Path): Boolean =
        os.isDir(path) && os.list(path).exists(isScalaCliFile)

      private def isScalaCliFile(path: os.Path): Boolean =
        os.isFile(path) &&
        path.last.endsWith(".sc") || (path.last.endsWith(".scala") &&
        os.read.lines(path)
          .iterator
          .dropWhile(_.startsWith("#!")) // FIXME Multi-line preambles??
          .filter(!_.trim.isEmpty)
          .takeWhile(_.startsWith("//> "))
          .hasNext)
    }
  }

  def discoverBuildTools(
    workspace: os.Path,
    currentFile: Option[os.Path],
    alreadyAdded: Set[plasmon.bsp.BuildTool]
  ): Seq[DiscoveredBuildTool] =
    BuildToolDiscover.all.flatMap(_.check(workspace, currentFile, alreadyAdded))

  final case class BspExtraBuildParams(
    javaSemanticdbVersion: String,
    semanticdbVersion: String,
    supportedScalaVersions: JList[String],
    enableBestEffortMode: Boolean
  )
}
