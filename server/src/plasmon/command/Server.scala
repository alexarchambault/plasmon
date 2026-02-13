package plasmon.command

import caseapp.core.RemainingArgs
import caseapp.core.app.CommandsEntryPoint
import org.eclipse.lsp4j.jsonrpc.Launcher
import org.eclipse.{lsp4j => l}
import plasmon.index.Indexer
import plasmon.internal.{Constants, DebugInput}
import plasmon.jsonrpc.{JsonrpcServer, NotificationHandler, RequestHandler}
import plasmon.protocol.{CommandClient, CommandServer}
import plasmon.servercommand.*
import plasmon.util.{ThreadUtil, TimerThreadsHack}
import plasmon.jsonrpc.Handlers
import plasmon.languageclient.PlasmonLanguageClient
import plasmon.{handlers => h, protocol}
import plasmon.PlasmonEnrichments.*

import java.net.{StandardProtocolFamily, URI, UnixDomainSocketAddress}
import java.nio.channels.{ClosedByInterruptException, ServerSocketChannel}
import java.nio.file.Paths
import java.util.Locale
import java.util.concurrent.{CompletableFuture, CountDownLatch, ExecutorService}

import scala.concurrent.duration.DurationInt
import scala.concurrent.duration.DoubleMult
import scala.jdk.CollectionConverters.*
import java.util.{Map => JMap}
import com.google.gson.JsonArray
import plasmon.jsonrpc.CommandHandler
import java.time.Instant
import scala.concurrent.duration.Duration
import scala.concurrent.duration.FiniteDuration
import java.time.ZoneId
import java.util.concurrent.ScheduledFuture
import plasmon.internal.BinaryName
import scala.concurrent.Future
import scala.util.Success
import scala.util.Failure
import scala.concurrent.ExecutionContext
import scala.annotation.nowarn
import scala.util.Properties
import plasmon.bsp.BuildTool
import com.google.gson.JsonDeserializer
import scala.concurrent.Await
import plasmon.watch.WatchEvent
import plasmon.pc.Scala2PresentationCompilerHandler
import plasmon.internal.DisableScala2Pc
import java.nio.file.Files
import java.io.RandomAccessFile
import scala.util.Using
import java.nio.charset.StandardCharsets

object Server extends caseapp.Command[ServerOptions] {

  def remoteCommands = Seq[ServerCommand[?]](
    About,
    Check,
    Import,
    Index,
    Inspect,
    Repl,
    ReplStop,
    BspAdd,
    BspBuildTargets,
    BspClean,
    BspCompile,
    BspList,
    BspReload,
    BspRemove,
    BspRequest,
    BuildToolAdd,
    Diagnostics,
    Exit,
    InteractiveUnload,
    LspCompletion,
    LspDefinition,
    LspHover,
    RefreshStatus,
    Reload,
    ShowIndex,
    SymbolSearch,
    Withdraw
  )

  def initLogging(logToFileOpt: Option[os.Path]): Unit = {
    import scribe.file.*
    import scribe.format.*
    import scribe.writer.*
    import scribe.*
    val format = formatter"$date $levelPaddedRight ${scribe.format.messages}"
    val writer = logToFileOpt match {
      case None => SystemErrWriter
      case Some(logToFile) =>
        FileWriter(pathBuilder = PathBuilder.static(logToFile.toNIO)).flushAlways
    }
    Logger.root
      .clearModifiers()
      .clearHandlers()
      .withHandler(
        writer = writer,
        formatter = format,
        minimumLevel = Some(Level.Info),
        modifiers = Nil
      )
      .replace()
  }

  override def main(progName: String, args: Array[String]): Unit =
    if (progName.isEmpty && args.headOption.flatMap(_.headOption).exists(_ != '-'))
      Command.main("", args)
    else
      try super.main(progName, args)
      finally TimerThreadsHack.cleanup()

  private lazy val isArm64 =
    Option(System.getProperty("os.arch")).map(_.toLowerCase(Locale.ROOT)).exists {
      case "aarch64" | "arm64" => true
      case _                   => false
    }
  def defaultJvmId =
    if (Properties.isWin && isArm64)
      "system|liberica:24"
    else
      "system|17"

  def run(options: ServerOptions, remainingArgs: RemainingArgs): Unit = {

    if (remainingArgs.all.nonEmpty) {
      System.err.println(
        s"Unexpected arguments passed to server command: ${remainingArgs.all.mkString(" ")}"
      )
      sys.exit(1)
    }

    val autoInit = options.autoInit.getOrElse(false)

    val heartBeatPeriodOpt =
      options.heartBeat.map(_.trim).filter(_.nonEmpty).map(Duration(_)).collect {
        case f: FiniteDuration => f
      }

    val workingDir = options.workingDir
      .filter(_.trim.nonEmpty)
      .map(os.Path(_, os.pwd))
      .getOrElse(os.pwd)

    withLockFile(workingDir / ".plasmon/lock") {

      // Sending something to stderr, so that an output channel for the server output gets created in vscode
      System.err.println(s"Plasmon ${Constants.version} starting")
      val isNativeImage = sys.props.contains("org.graalvm.nativeimage.imagecode")
      System.err.println(
        if (isNativeImage)
          s"Running on native-image (executable path: ${BinaryName.pathOpt.map(_.toString).getOrElse("[unknown]")})"
        else
          "Running on " +
            sys.props.get("java.vendor").toSeq
              .++(sys.props.get("java.vm.name").toSeq)
              .mkString(" ") +
            " " + sys.props("java.version") +
            s" (server JAR URI: ${getClass.getProtectionDomain.getCodeSource.getLocation.toURI})"
      )

      initLogging {
        if (options.logToStderr.getOrElse(autoInit)) None
        else Some(workingDir / ".plasmon/log")
      }

      scribe.info("Starting from scribe")

      scribe.info(s"Options: $options")

      scribe.info(s"Default locale: ${Locale.getDefault}")

      var workspaceOpt        = Option.empty[os.Path]
      var initializeParamsOpt = Option.empty[l.InitializeParams]

      val pools = ServerCommandThreadPools.noobNaiveDontUsePools()

      val javaHome = options.javaHome.filter(_.trim.nonEmpty).map(os.Path(_, os.pwd))
        .getOrElse {
          val jvmId = options.jvm.map(_.trim).filter(_.nonEmpty).getOrElse(defaultJvmId)
          os.Path(coursierapi.JvmManager.create().get(jvmId), os.pwd)
        }

      val bloopJavaHome = () => {
        val value = options.bloopJavaHome.filter(_.trim.nonEmpty).map(os.Path(_, os.pwd))
          .orElse {
            options.bloopJvm.map(_.trim).filter(_.nonEmpty).map { jvmId =>
              os.Path(coursierapi.JvmManager.create().get(jvmId), os.pwd)
            }
          }
          .getOrElse {
            // FIXME Detect if that's < Java 17, and download a hard-coded JDK 17 instead
            javaHome
          }
        scribe.info(s"Bloop Java home: $value")
        value
      }

      scribe.info(s"Java home: $javaHome")
      scribe.info(
        s"Location: ${Option(getClass.getProtectionDomain.getCodeSource).map(_.getLocation).orNull}"
      )

      Locale.setDefault(Locale.US)

      val formerJavaHome = System.getProperty("java.home")
      try {
        System.setProperty("java.home", javaHome.toString)
        javax.tools.ToolProvider.getSystemJavaCompiler
          .getStandardFileManager(null, null, null)
      }
      finally
        if (formerJavaHome == null)
          System.clearProperty("java.home")
        else
          System.setProperty("java.home", formerJavaHome)

      val tools = BuildTool.Tools(Map(
        "scala-cli" -> {
          options.scalaCli match {
            case None =>
              if (Properties.isWin) Seq("scala-cli.exe")
              else Seq("scala-cli")
            case Some(value) if value.startsWith("[") =>
              // TODO Parse as JSON array
              ???
            case Some(value) =>
              Seq(value)
          }
        }
      ))

      val scala2Compat =
        if (!Constants.disableScala2Pc || new Scala2PresentationCompilerHandler().available()) {
          scribe.info("Scala 2 PC available")
          options.scala2Compat match {
            case Some(value) =>
              scribe.info(
                if (value) "Scala 2 PC enabled via command-line"
                else "Scala 2 PC disabled via command-line"
              )
              value
            case None =>
              val compat = Constants.disableScala2Pc || (new DisableScala2Pc()).getAsBoolean
              scribe.info(
                if (compat) "Disabling Scala 2 PC use"
                else "Enabling Scala 2 PC use"
              )
              compat
          }
        }
        else {
          scribe.info("Scala 2 PC not available")
          if (options.scala2Compat.contains(false))
            scribe.warn("Scala 2 PC use requested on command-line, but Scala 2 PC not available")
          true
        }

      var indexer: Indexer = null
      lazy val server: plasmon.Server = new plasmon.Server(
        javaHome,
        bloopJavaHome,
        () => workspaceOpt,
        pools.serverPools,
        workingDir = workingDir,
        onBuildTargetDidChange = { params =>
          if (options.ignoreBuildTargetDidChange)
            scribe.info(s"Ignoring build target did change notification $params")
          else {
            scribe.info(s"Queueing re-index request upon build target change $params")
            server.fileWatcher.enqueue(WatchEvent.Reindex)
          }
        },
        reIndex = { () =>
          server.languageClient.buildChangeDetected(
            PlasmonLanguageClient.BuildChangeDetails()
          )
        },
        logJsonrpcInput = options.logJsonrpcInput.getOrElse(false),
        tools = tools,
        enableBestEffortMode = options.bestEffort.getOrElse(true),
        reindexSource = indexer.reindexWorkspaceSource(_),
        scala2Compat = scala2Compat
      )

      indexer = new Indexer(server)
      server.indexerLogger = Some(indexer.logger)
      indexer.onStateChange { (indexerState, loggerOpt) =>
        server.updateIndexerState(indexerState, loggerOpt)
      }

      var lastClientHeartBeatTime = Option.empty[Instant]

      def heartBeatRunnable(period: FiniteDuration, onMissingClient: Instant => Unit): Runnable = {

        val maxElapsed = 2.5 * period

        () =>
          try {
            val now = Instant.now()
            val noMoreClientSinceOpt = lastClientHeartBeatTime.filter { lastHeartBeatTime =>
              val elapsed = (now.toEpochMilli() - lastHeartBeatTime.toEpochMilli()).toInt.millis
              elapsed > maxElapsed
            }
            noMoreClientSinceOpt match {
              case Some(noMoreClientSince) =>
                onMissingClient(noMoreClientSince)
              case None =>
                scribe.info("Sending heart beat request")
                server.languageClient.heartBeat()
            }
          }
          catch {
            case t: Throwable =>
              scribe.error("Caught exception during heart beat check, ignoring it", t)
          }
      }

      def init(params: l.InitializeParams, commands: Seq[String]): l.InitializeResult = {
        val updateWorkspaceOpt = {
          val viaFoldersOpt = Option(params.getWorkspaceFolders)
            .flatMap(_.asScala.headOption)
            .map(_.getUri.osPathFromUri)
          def viaRootUriOpt = {
            @nowarn
            def rootUri = params.getRootUri
            Option(rootUri).map(_.osPathFromUri)
          }
          @nowarn
          def rootPath = params.getRootPath
          def viaRootPathOpt = Option(rootPath)
            .map(Paths.get(_).maybeToRealPath)
            .map(os.Path(_))
          viaFoldersOpt.orElse(viaRootUriOpt).orElse(viaRootPathOpt)
        }
        workspaceOpt = updateWorkspaceOpt.orElse(workspaceOpt)
        initializeParamsOpt = Some(params)
        server.setInitializeParams(params)

        val cap = new l.ServerCapabilities
        cap.setTextDocumentSync {
          val textDocumentSyncOptions = new l.TextDocumentSyncOptions
          textDocumentSyncOptions.setChange(l.TextDocumentSyncKind.Full)
          textDocumentSyncOptions.setSave(new l.SaveOptions(true))
          textDocumentSyncOptions.setOpenClose(true)
          textDocumentSyncOptions
        }
        cap.setDocumentOnTypeFormattingProvider(
          new l.DocumentOnTypeFormattingOptions("\n", List("\"", "{").asJava)
        )
        cap.setDefinitionProvider(true)
        cap.setReferencesProvider(true)
        cap.setHoverProvider(true)
        cap.setCompletionProvider(new l.CompletionOptions(true, List(".", "*").asJava))
        cap.setCodeLensProvider(new l.CodeLensOptions(false))
        cap.setExecuteCommandProvider(new l.ExecuteCommandOptions(commands.toList.asJava))
        cap.setSignatureHelpProvider(new l.SignatureHelpOptions(List("(", "[", ",").asJava))

        server.refreshStatus()

        if (options.importPersistedTargets) {
          val loadBsp = server.bspServers.loadFromDisk(
            scribe.info(_),
            pools.bspEces,
            () => pools.bloopThreads
          )
          def loadModules = indexer.loadFromDisk(
            toplevelCacheOnly = false,
            ignoreToplevelSymbolsErrors = true,
            mayReadFromBspCache = true
          )
          val init = {
            implicit val ec: ExecutionContext = pools.dummyEc
            for {
              _ <- loadBsp
              _ <- loadModules
              _ <- Future {
                server.refreshStatus()
              }
            } yield ()
          }
          init.onComplete {
            case Success(()) =>
            case Failure(ex) =>
              scribe.error("Error initializing server", ex)
          }(using pools.dummyEc)
        }

        if (Option(params.getCapabilities.getWorkspace).forall(_.getDidChangeWatchedFiles == null))
          scribe.info("Client doesn't support file watching")

        for (heartBeatPeriod <- heartBeatPeriodOpt) {
          var f: ScheduledFuture[?] = null
          val runnable = heartBeatRunnable(
            heartBeatPeriod,
            noMoreClientSince => {
              scribe.warn(
                s"No heart beat from client since ${noMoreClientSince.atZone(ZoneId.systemDefault()).toLocalDateTime()}, stopping server"
              )
              server.exit()
              f.cancel(false)
            }
          )
          lastClientHeartBeatTime = Some(Instant.now())
          f = pools.clientHealthCheckScheduler.scheduleAtFixedRate(
            runnable,
            heartBeatPeriod.length,
            heartBeatPeriod.length,
            heartBeatPeriod.unit
          )
        }

        new l.InitializeResult(cap)
      }

      def baseHandlers(commands: Seq[String]) = Handlers(
        notificationHandlers = Nil,
        requestHandlers = Seq(
          RequestHandler.of[l.InitializeParams, l.InitializeResult]("initialize") {
            (params, logger) =>
              val res = init(params, commands)
              CompletableFuture.completedFuture(res)
          }
        ),
        commandHandlers = Nil
      )

      val handlers = Handlers.sum(
        h.Hover.handlers(
          server,
          pools.cancelTokensEces,
          pools.hoverStuffEc
        ),
        h.Definition.handlers(
          server,
          pools.cancelTokensEces,
          pools.definitionStuffEc
        ),
        h.ref.Reference.handlers(
          server,
          pools.definitionStuffEc
        ),
        h.Completion.handlers(server, pools.cancelTokensEces),
        h.codelens.CodeLens.handlers(server),
        h.SetTrace.handlers(),
        h.docchange.DocumentChange.handlers(server),
        h.SignatureHelp.handlers(server, pools.cancelTokensEces),
        h.ontypefmt.OnTypeFormatting.handlers(server, server.pools.onTypeFormattingEc),
        h.PlasmonCommands.handlers(server, indexer, pools),
        Handlers(
          Seq(
            NotificationHandler.of[Boolean]("metals/windowStateDidChange") { (focused, logger) =>
              server.refreshStatus()
              if (options.suspendWatcher)
                if (focused) {
                  logger.log("Focused")
                  server.fileWatcher.resume()
                }
                else {
                  logger.log("Not focused")
                  server.fileWatcher.suspend()
                }
            },
            NotificationHandler.of[l.DidChangeConfigurationParams](
              "workspace/didChangeConfiguration"
            )((_, _) => ()),
            NotificationHandler.of[Object]("metals/didFocusTextDocument") { (param, logger) =>
              // adapted from Metals didFocus stuff
              val uriOpt = param match {
                case str: String => Some(str)
                case arr: JsonArray
                    if arr.size() == 1 && arr.get(0).isJsonPrimitive && arr.get(
                      0
                    ).getAsJsonPrimitive.isString => Some(arr.get(0).getAsJsonPrimitive.getAsString)
                case l: java.util.List[_] if l.size() == 1 && l.get(0).isInstanceOf[String] =>
                  Some(l.get(0).asInstanceOf[String])
                case (str: String) :: Nil => Some(str)
                case _ =>
                  scribe.warn(
                    s"Unexpected notification params received for didFocusTextDocument: $param" +
                      (if (param == null) "" else s" (${param.getClass})")
                  )
                  None
              }
              for (uri <- uriOpt)
                server.editorState.focusedDocument = Some(uri.osPathFromUri)
              server.refreshStatus()
            },
            NotificationHandler.of[l.InitializedParams]("initialized") { (_, _) =>
            },
            NotificationHandler.of[Unit]("exit") { (_, _) =>
              server.exit()
            }
          ),
          Seq(
            RequestHandler.of[Unit, Object]("shutdown") { (_, _) =>
              server.shutdown().thenApply(_ => null)
            }
          ),
          Seq(
            CommandHandler.of("plasmon/clientHeartBeat") { (_, _) =>
              lastClientHeartBeatTime = Some(Instant.now())
              CompletableFuture.completedFuture(null)
            },
            CommandHandler.of("plasmon/showSymbolIndex") { (_, _) =>
              CompletableFuture.completedFuture(null)
            }
          )
        )
      )

      val commandNames = handlers.commandHandlers.map(_.commandName)

      if (autoInit) {
        scribe.info("Auto-initializing server")
        val params = new l.InitializeParams
        params.setWorkspaceFolders(
          List(
            new l.WorkspaceFolder(
              os.pwd.toNIO.toUri.toASCIIString,
              os.pwd.lastOpt.getOrElse("root")
            )
          ).asJava
        )
        init(params, commandNames)
      }

      val lspServer = new JsonrpcServer(
        Handlers.sum(
          baseHandlers(commandNames),
          h.ExecuteCommand.handlers(server, handlers.commandHandlers),
          handlers
        ),
        server.loggerManager.create("lsp-requests", "Requests")
      )

      def commandServer(pool: ExecutorService): CommandServer =
        new CommandServerImpl(
          client => remoteCommands.map(_.instance(server, indexer, client, lspServer, pools)),
          pool
        )

      val commandSocketPath =
        options.socket.filter(_.trim.nonEmpty)
          .map(os.Path(_, os.pwd))
          .getOrElse(workingDir / Command.socketPath)

      scribe.info(s"commandSocketPath=$commandSocketPath")
      val actualCommandSocketPath = Command.actualSocket(commandSocketPath, isServer = true)
      if (!os.exists(actualCommandSocketPath / os.up))
        os.makeDir.all(actualCommandSocketPath / os.up)
      scribe.info(s"actualCommandSocketPath=$actualCommandSocketPath")
      Runtime.getRuntime.addShutdownHook(
        new Thread("cleanup-socket-file") {
          setDaemon(true)
          override def run(): Unit =
            os.remove(actualCommandSocketPath)
        }
      )
      val addr                       = UnixDomainSocketAddress.of(actualCommandSocketPath.toNIO)
      val commandServerSocketChannel = ServerSocketChannel.open(StandardProtocolFamily.UNIX)
      scribe.info(s"Listening for commands on $addr")
      commandServerSocketChannel.bind(addr)

      val commandServerThread: Thread = new Thread("command-server") {
        setDaemon(true)
        override def run(): Unit =
          try
            ThreadUtil.withFixedThreadPool("plasmon-commands-jsonrpc", 4) { pool =>
              ThreadUtil.withFixedThreadPool("plasmon-commands", 4) { runCommandPool =>

                val commandServer0 = commandServer(runCommandPool)

                while (true) {
                  val clientSocketChannel = commandServerSocketChannel.accept()
                  val clientSocket        = libdaemonjvm.Util.socketFromChannel(clientSocketChannel)

                  val (input, output) =
                    if (options.logJsonrpcInput.getOrElse(false))
                      DebugInput.debug(
                        clientSocket.getInputStream,
                        clientSocket.getOutputStream,
                        (line, isOut) =>
                          scribe.info(s"Command server ${if (isOut) ">" else "<"} $line")
                      )
                    else
                      (clientSocket.getInputStream, clientSocket.getOutputStream)

                  val launcher = new Launcher.Builder[CommandClient]()
                    .setExecutorService(pool)
                    .setInput(input)
                    .setOutput(output)
                    .setRemoteInterface(classOf[CommandClient])
                    .setLocalService(commandServer0)
                    .setExceptionHandler { t =>
                      scribe.info("Error during command processing", t)
                      l.jsonrpc.RemoteEndpoint.DEFAULT_EXCEPTION_HANDLER.apply(t)
                    }
                    .create()

                  val remoteClient = launcher.getRemoteProxy
                  commandServer0.setClient(remoteClient)

                  launcher.startListening()
                }
              }
            }
          catch {
            case _: ClosedByInterruptException =>
              scribe.info("Command server thread exiting on interruption")
            case _: InterruptedException =>
              scribe.info("Command server thread exiting on interruption")
            case e: Throwable =>
              scribe.error("Exception in command server thread", e)
              throw e
          }
      }

      server.onShutdown = server.onShutdown :+ { () =>
        if (commandServerThread.isAlive())
          commandServerThread.interrupt()
      }
      commandServerThread.start()

      val done = new CountDownLatch(1)

      server.onExit = server.onExit :+ { () =>
        if (done.getCount > 0)
          done.countDown()
      }

      val currentOut = System.out
      Console.withOut(System.err) {
        try {
          System.setOut(System.err)
          ThreadUtil.withFixedThreadPool("plasmon-jsonrpc", 4) { pool =>

            val (serverInput, serverOutput) =
              if (options.logJsonrpcInput.getOrElse(false))
                DebugInput.debug(
                  System.in,
                  currentOut,
                  (line, isOut) => scribe.info(s"LSP ${if (isOut) ">" else "<"} $line")
                )
              else
                (System.in, currentOut)

            val builder: Launcher.Builder[PlasmonLanguageClient] =
              new Launcher.Builder[PlasmonLanguageClient] {
                def endpointOpt(): Option[l.jsonrpc.Endpoint] =
                  localServices.asScala.toVector match {
                    case Seq(e: l.jsonrpc.Endpoint) => Some(e)
                    case _                          => None
                  }
                override def createRemoteEndpoint(jsonHandler: l.jsonrpc.json.MessageJsonHandler)
                  : l.jsonrpc.RemoteEndpoint =
                  endpointOpt() match {
                    case Some(localEndpoint) =>
                      // same as https://github.com/eclipse-lsp4j/lsp4j/blob/v0.20.1/org.eclipse.lsp4j.jsonrpc/src/main/java/org/eclipse/lsp4j/jsonrpc/Launcher.java#L345-L356,
                      // but for the local endpoint creation
                      val remoteEndpoint = new l.jsonrpc.RemoteEndpoint(
                        wrapMessageConsumer(
                          new l.jsonrpc.json.StreamMessageConsumer(output, jsonHandler)
                        ),
                        localEndpoint,
                        Option(exceptionHandler)
                          .getOrElse(l.jsonrpc.RemoteEndpoint.DEFAULT_EXCEPTION_HANDLER)
                      )
                      jsonHandler.setMethodProvider(remoteEndpoint)
                      remoteEndpoint
                    case None =>
                      super.createRemoteEndpoint(jsonHandler)
                  }
                override def getSupportedMethods: JMap[String, l.jsonrpc.json.JsonRpcMethod] =
                  if (endpointOpt().isDefined)
                    lspServer.supportedMethods.asJava
                  else
                    super.getSupportedMethods
              }
            val launcher = builder
              .setExecutorService(pool)
              .setInput(serverInput)
              .setOutput(serverOutput)
              .setRemoteInterface(classOf[PlasmonLanguageClient])
              .setLocalService(lspServer)
              .configureGson { gsonBuilder =>
                val d: JsonDeserializer[Void] = (json, tpe, ctx) => null
                gsonBuilder.registerTypeAdapter(classOf[Void], d)
                gsonBuilder.registerTypeAdapter(Void.TYPE, d)
              }
              .setExceptionHandler { t =>
                scribe.info("Error during LSP processing", t)
                l.jsonrpc.RemoteEndpoint.DEFAULT_EXCEPTION_HANDLER.apply(t)
              }
              .create()

            val remoteClient = launcher.getRemoteProxy
            server.setClient(remoteClient)

            ThreadUtil.withCachedThreadPool("plasmon-server-exit-watcher") { exitPool =>
              implicit val ec = ExecutionContext.fromExecutorService(exitPool)
              val stdinFuture = Future {
                launcher.startListening().get(); "stdin"
              }
              val exitFuture = Future {
                done.await()
                "exit"
              }
              val first = Future.firstCompletedOf(Seq(stdinFuture, exitFuture))
              val res   = Await.result(first, Duration.Inf)
              scribe.info(s"Exited because of $res")
            }

            server.close()
          }
        }
        finally
          System.setOut(currentOut)
      }
    }
  }

  private def withLockFile[T](path: os.Path)(f: => T): T = {
    val pid = ProcessHandle.current().pid()
    System.err.println(s"PID: $pid")
    System.err.println(s"Waiting for lock on $path")
    os.makeDir.all(path / os.up)
    Using.resource(new RandomAccessFile(path.toIO, "rw")) { raf =>
      System.err.println("Opened lock file")
      Using.resource(raf.getChannel.lock()) { lock =>
        System.err.println("Lock acquired")
        raf.setLength(0L)
        raf.write(s"pid=$pid".getBytes(StandardCharsets.UTF_8))

        try f
        finally raf.setLength(0L)
      }
    }
  }
}
