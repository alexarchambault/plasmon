package plasmon

import ch.epfl.scala.{bsp4j => b}
import org.eclipse.{lsp4j => l}
import plasmon.bsp.{BspServers, PlasmonBuildClientImpl}
import plasmon.languageclient._

import java.nio.file.NoSuchFileException
import java.util.concurrent.{CompletableFuture, TimeUnit}

import scala.concurrent.Future
import scala.meta.internal.metals.{Docstrings, ExcludedPackagesHandler}
import scala.meta.internal.mtags.{IndexingExceptions, Mtags, OnDemandSymbolIndex, OpenClassLoader}
import scala.meta.parsers.ParseException
import scala.meta.tokenizers.TokenizeException
import scala.meta.internal.mtags.SourcePath
import plasmon.watch.{FileWatcher, ProjectFileWatcher, WatchEvent}
import scala.util.Success
import scala.util.Failure
import plasmon.bsp.BuildTool
import plasmon.index.IndexerServerLike
import plasmon.bsp.BspServersServerLike
import plasmon.status.StatusActor
import scala.concurrent.Promise
import scala.concurrent.duration.DurationInt
import scala.meta.internal.pc.CustomFileManager
import scala.meta.internal.pc.JavaMetalsGlobal

import plasmon.ide._

import plasmon.PlasmonEnrichments._
import scala.jdk.CollectionConverters._
import scala.meta.internal.metals.ClasspathSearch

import plasmon.index.{BspData, ReferenceIndex, SymbolSearchIndex}
import plasmon.pc.PresentationCompilers
import plasmon.semdb.{
  AggregateSemanticdbs,
  FileSystemSemanticdbs,
  InteractiveSemanticdbs,
  JavaInteractiveSemanticdb,
  SemanticdbIndexer
}
import plasmon.pc.NopReportContext
import scala.meta.internal.pc.PresentationCompilerConfigImpl
import scala.meta.internal.metals.JdkSources
import scala.meta.internal.semanticdb.TextDocument
import scala.meta.internal.mtags.GlobalSymbolIndex

// Many things here inspired by https://github.com/scalameta/metals/blob/030641d97ca5b982144898a54c3b60b2c08b9614/metals/src/main/scala/scala/meta/metals/MetalsLanguageServer.scala
// and earlier version of that file

/** All that's needed to answer official / spec LSP requests
  *
  * This knows nothing about build import, BSP servers, etc. This only interacts with builds via
  * `bspData` and `isCurrentlyCompiling`.
  */
final class Server(
  val javaHome: os.Path,
  val bloopJavaHome: () => os.Path,
  val workspaceOpt: () => Option[os.Path],
  val pools: ServerThreadPools,
  val workingDir: os.Path,
  onBuildTargetDidChange: b.DidChangeBuildTarget => Unit,
  reIndex: () => Unit,
  val logJsonrpcInput: Boolean,
  val tools: BuildTool.Tools,
  val enableBestEffortMode: Boolean,
  val reindexSource: os.Path => Unit
) extends IndexerServerLike with BspServersServerLike with AutoCloseable {

  private var initializeParamsOpt0 = Option.empty[l.InitializeParams]
  def setInitializeParams(params: l.InitializeParams): Unit = {
    initializeParamsOpt0 = Some(params)

    if (isLanguageClientInstantiated)
      languageClient.underlying = underlyingLanguageClient()
  }

  var onShutdown = Seq.empty[() => Unit]
  var onExit     = Seq.empty[() => Unit]

  def shutdown(): CompletableFuture[Unit] = {
    onShutdown.foreach(_())
    var exited = false
    onExit = onExit :+ { () =>
      exited = true
    }
    // not the right scheduler…
    pools.pcThreadStopScheduler.schedule(
      new Runnable {
        def run(): Unit =
          if (!exited)
            onExit.foreach(_())
      },
      1L,
      TimeUnit.SECONDS
    )
    CompletableFuture.completedFuture(())
  }

  def exit(): Unit = {
    onExit.foreach(_())
  }

  private var indexerState0     = List.empty[String]
  private var indexerLoggerOpt0 = Option.empty[Logger]
  def updateIndexerState(state: List[String], loggerOpt: Option[Logger]): Unit = {
    indexerState0 = state
    indexerLoggerOpt0 = loggerOpt
    refreshStatus()
  }
  def currentIndexerState(): (List[String], Option[Logger]) =
    (indexerState0, indexerLoggerOpt0)

  // STATE
  lazy val bspServers: BspServers = {
    val instance = new BspServers(
      Some(workingDir / ".plasmon/build-servers.json"),
      this,
      pools.bspServersActorContext
    )
    instance.onStateChange { (_, _) =>
      refreshStatus()
    }
    instance
  }

  // STATE
  // can't use workspace value here?
  val bspData: BspData = new BspData(os.pwd, bspServers)

  // stateful
  private var isLanguageClientInstantiated = false
  private def underlyingLanguageClient(): PlasmonLanguageClient =
    (clientOpt, initializeParamsOpt0) match {
      case (Some(client), Some(initializeParams)) =>
        new PlasmonConfiguredLanguageClient(client, initializeParams)(pools.configLcEc)
      case _ =>
        PlasmonNoopLanguageClient
    }
  lazy val languageClient: PlasmonDelegatingLanguageClient = {
    isLanguageClientInstantiated = true
    new PlasmonDelegatingLanguageClient(underlyingLanguageClient())
  }

  val workspace: () => os.Path =
    () => workspaceOpt().getOrElse(sys.error("no workspace"))

  private var clientOpt: Option[PlasmonLanguageClient] = None
  def setClient(client: PlasmonLanguageClient): Unit = {
    clientOpt = Some(client)
    if (isLanguageClientInstantiated)
      languageClient.underlying = underlyingLanguageClient()
  }

  val loggerManager = Logger.Manager.create { channel =>
    // send empty line to trigger output channel creation on client
    for (client <- clientOpt)
      client.log(
        PlasmonLanguageClient.LogMessage(
          channel.id,
          channel.label,
          List().asJava
        )
      )

    line =>
      val clientOpt0 = clientOpt

      val extra = if (clientOpt0.isEmpty) " (not sent to client)" else ""
      scribe.info(s"${channel.label}$extra: $line")

      // FIXME Keep messages until we have a client?
      for (client <- clientOpt0)
        client.log(
          PlasmonLanguageClient.LogMessage(
            channel.id,
            channel.label,
            List(line).asJava
          )
        )
  }

  // STATE
  lazy val editorState: EditorState = {
    val buffers = Buffers()
    new EditorState(
      buffers,
      None,
      new MutableMd5Fingerprints,
      new Trees(buffers, bspData)
    )
  }

  // STATE
  lazy val compilations = new Compilations(
    bspData,
    afterSuccessfulCompilation = postCompilation(_),
    onStartCompilation = () => {
      refreshStatus()
      scribe.info("nothing on start compilation (not implemented)")
    },
    bestEffortEnabled = enableBestEffortMode
  )(pools.compilationEc)

  lazy val jdkCp = {
    val rtJar = javaHome / "jre/lib/rt.jar"
    if (os.exists(rtJar)) Seq(rtJar)
    else {
      val jmodsDir = javaHome / "jmods"
      if (os.isDir(jmodsDir))
        os.list(jmodsDir)
          .filter(_.last.endsWith(".jmod"))
          .filter(os.isFile(_))
      else {
        scribe.warn(s"No rt.jar or jmods directory found in Java home dir $javaHome")
        Nil
      }
    }
  }

  lazy val jdkSources =
    JdkSources(Some(javaHome.toString)) match {
      case Left(err) =>
        scribe.warn(
          s"No JDK sources found under $javaHome (tried ${err.candidates.mkString(", ")})"
        )
        None
      case Right(path) =>
        Some(path.toOs)
    }

  private lazy val jdkContext = new SourcePath.Context

  private lazy val javaFileManager = new CustomFileManager(
    javaHome.toNIO,
    JavaMetalsGlobal.COMPILER.getStandardFileManager(null, null, null),
    jdkContext
  )

  private lazy val presentationCompilerConfig: PresentationCompilerConfigImpl =
    PresentationCompilerConfigImpl()

  // STATE
  lazy val presentationCompilers: PresentationCompilers = new PresentationCompilers(
    workspace = workingDir, // TODO by-name
    javaHome = javaHome,
    javaFileManager = () => javaFileManager,
    config0 = presentationCompilerConfig,
    bspData = bspData,
    buffers = editorState.buffers,
    symbolDocs = symbolDocs,
    symbolSearchIndex = symbolSearchIndex,
    patchedSymbolIndex = patchedSymbolIndex,
    index = symbolIndex,
    sh = pools.pcThreadStopScheduler,
    completionItemPriority = () => referenceIndex.completionItemPriority,
    javaUserLoggerMaker = targetId =>
      loggerManager.create(
        s"java-presentation-compiler-$targetId",
        s"Java Presentation Compiler $targetId"
      ).consumer,
    loggerManager = loggerManager,
    refreshStatus = refreshStatus,
    languageClient = languageClient
  )(pools.compilerEces)

  // stateless
  def patchedSymbolIndex: PatchedSymbolIndex = new PatchedSymbolIndex(
    symbolIndex,
    workingDir,
    editorState.buffers,
    () => semanticdbs,
    editorState.trees,
    saveDefFileToDisk = true,
    bspData
  )

  def createBuildClient(): PlasmonBuildClientImpl = {
    val client = new PlasmonBuildClientImpl(
      languageClient,
      editorState.buffers,
      editorState.trees,
      workspace(),
      onBuildTargetDidChangeFunc = params =>
        if (params.getChanges.asScala.nonEmpty) {
          scribe.info(s"Some build targets changed ($params)")
          onBuildTargetDidChange(params)
        }
    )
    client.onProgress { _ =>
      refreshStatus()
    }
    client
  }

  // STATE
  lazy val parserQueue = new ParserQueue(editorState.buffers, editorState.trees)

  // stateless
  private lazy val javaInteractiveSemanticdb =
    JavaInteractiveSemanticdb.create(
      () => javaFileManager,
      workspace() /* TODO by-name */,
      workingDir,
      bspData,
      loggerManager.create("java-interactive", "Java Interactive").consumer
    )

  def computeInteractiveSemanticdb(
    path: os.Path,
    source: String,
    targetId: b.BuildTargetIdentifier
  ): TextDocument =
    if (path.isJavaFilename)
      javaInteractiveSemanticdb.textDocument(path, source, Some(targetId))
    else
      presentationCompilers.semanticdbTextDocument(path, source, targetId)

  // STATE
  // EVENT SOURCE: new semanticdb available for file
  lazy val interactiveSemanticdbs: InteractiveSemanticdbs =
    new InteractiveSemanticdbs(
      workingDir,
      editorState.buffers,
      onNewSemanticdb = onNewSemanticdb(_, _, _),
      computeInteractiveSemanticdb = computeInteractiveSemanticdb(_, _, _)
    )

  // stateless
  def fileSystemSemanticdbs = new FileSystemSemanticdbs(bspData, editorState.fingerprints)

  // stateless
  def semanticdbs: AggregateSemanticdbs = AggregateSemanticdbs(
    List(
      fileSystemSemanticdbs,
      interactiveSemanticdbs
    )
  )

  // STATE
  // EVENT: upon unloading targets, do some clean-up
  lazy val referenceIndex = new ReferenceIndex(
    workingDir,
    () => semanticdbs,
    editorState.buffers,
    symbolIndex,
    editorState.trees,
    bspData,
    presentationCompilers
  )(pools.referenceProviderEc)

  var indexerLogger = Option.empty[Logger]

  // stateless
  private def semanticDBIndexer = new SemanticdbIndexer(
    List(
      referenceIndex
      // implementationProvider,
      // testProvider
    ),
    bspData,
    workspace(), // TODO by-name
    indexerLogger.map(_.consumer).getOrElse(_ => ())
  )

  // STATE
  lazy val symbolIndex: OnDemandSymbolIndex =
    OnDemandSymbolIndex.empty(
      javaHome.toNIO,
      new Mtags()(NopReportContext),
      onError = {
        case e @ (_: ParseException | _: TokenizeException) =>
          scribe.error("Ignoring parsing error", e)
        case e: IndexingExceptions.InvalidJarException =>
          scribe.warn(s"Ignoring invalid jar error for ${e.path}", e)
        case e: IndexingExceptions.PathIndexingException =>
          scribe.error(s"Ignoring issues while parsing: ${e.path}", e)
        case e: IndexingExceptions.InvalidSymbolException =>
          scribe.error(s"Ignoring error while searching for `${e.symbol}`", e)
        case e: NoSuchFileException =>
          // only comes for badly configured jar with `/Users` path added.
          scribe.warn("Ignoring index error", e)
        case e: Throwable =>
          scribe.error("Ignoring unexpected error during source scanning", e)
      },
      sourceJars = () => new OpenClassLoader,
      toIndexSource = path => bspData.mappedTo(path.toOs).map(_.path).getOrElse(path.toOs).toAbsPath
    )(NopReportContext)

  // STATE
  lazy val symbolSearchIndex = new SymbolSearchIndex(
    workingDir,
    javaHome,
    bspData,
    index = symbolIndex,
    saveClassFileToDisk = true,
    excludedPackageHandler = () => ExcludedPackagesHandler.default,
    classpathSearchIndexer = ClasspathSearch.Indexer.default
  )

  // STATE
  lazy val symbolDocs = new Docstrings(symbolIndex)(NopReportContext)

  val status = new Status(this, pools.bspHealthCheckScheduler)

  val statusActor = new StatusActor(
    languageClient,
    status,
    30.milliseconds,
    pools.statusActorScheduler
  )(
    pools.statusActorContext
  )

  def refreshStatusDetails(): Future[Option[(os.Path, Seq[PlasmonLanguageClient.StatusUpdate])]] = {
    val p = Promise[Option[(os.Path, Seq[PlasmonLanguageClient.StatusUpdate])]]()
    statusActor.send(
      StatusActor.Message.RefreshStatus(
        onDone = Some(res => p.complete(res))
      )
    )
    p.future
  }

  def refreshStatus(): Unit =
    refreshStatusDetails().onComplete {
      case Success(_) =>
      case Failure(ex) =>
        scribe.error("Failed to refresh status", ex)
    }(pools.dummyEc)

  def close(): Unit = {
    scribe.info(s"Closing $this")
    bspServers.close()
  }

  lazy val fileWatcher: FileWatcher =
    new ProjectFileWatcher(
      () => workingDir,
      bspData,
      watchFilter = {
        val bspDir = workingDir / ".bsp"
        path =>
          val name = path.last
          name.endsWith(".scala") || name.endsWith(".java") || name.endsWith(".semanticdb") ||
          path.startsWith(bspDir)
      },
      preprocessWatchEvent = {
        case event: WatchEvent.CreateOrModify
            if event.path.isScalaOrJava && !os.isDir(event.path) =>
          if (!editorState.buffers.contains(event.path))
            fileChangedOrCreatedUpdateState(event.path, true)
        case event: WatchEvent.Overflow if event.path.isScalaOrJava && !os.isDir(event.path) =>
          if (!editorState.buffers.contains(event.path))
            fileChangedOrCreatedUpdateState(event.path, false)
      },
      onFileWatchEvent = {
        case event: WatchEvent.Delete if event.path.isScalaOrJava =>
          fileDeletedInternal(event.path)
        case event: WatchEvent.CreateOrModify
            if event.path.isScalaOrJava && !os.isDir(event.path) =>
          if (!editorState.buffers.contains(event.path))
            fileChangedOrCreatedInternal(event.path, true)
        case event: WatchEvent.Overflow if event.path.isScalaOrJava && !os.isDir(event.path) =>
          if (!editorState.buffers.contains(event.path))
            fileChangedOrCreatedInternal(event.path, false)
        case event: WatchEvent.CreateOrModify if event.path.isSemanticdb =>
          Future {
            val it =
              bspData
                .allScala
                .filter(target => event.path.startsWith(target.classDirectory))
                .map(_.id) ++
                bspData
                  .allJava
                  .filter(target => event.path.startsWith(target.classDirectory))
                  .map(_.id)
            for (targetId <- it)
              semanticDBIndexer.onChange(
                GlobalSymbolIndex.BuildTarget(targetId.getUri),
                event.path
              )
          }(pools.indexingEc).onComplete {
            case Success(_) =>
            case Failure(ex) =>
              scribe.error(s"Error updating semanticdb for ${event.path}", ex)
          }(pools.indexingEc)
        case event: WatchEvent.Delete if event.path.isSemanticdb =>
          Future {
            semanticDBIndexer.onDelete(event.path)
          }(pools.indexingEc).onComplete {
            case Success(_) =>
            case Failure(ex) =>
              scribe.error(s"Error updating semanticdb for ${event.path}", ex)
          }(pools.indexingEc)
        case event: WatchEvent.Overflow if event.path.isSemanticdb =>
          Future {
            semanticDBIndexer.onOverflow(event.path)
          }(pools.indexingEc).onComplete {
            case Success(_) =>
            case Failure(ex) =>
              scribe.error(s"Error updating semanticdb for ${event.path}", ex)
          }(pools.indexingEc)
        case WatchEvent.Reindex =>
          reIndex()
      }
    )(pools.fileWatcherEc)

  def editorFileOpened(path: os.Path, currentContent: String, contentVersion: Int): Unit = {
    editorState.updateFocusedDocument(path, os.read(path), currentContent)
    refreshStatus()

    def interactive =
      bspData.inverseSources(path).foreach { target =>
        interactiveSemanticdbs.textDocument(path, target.module)
      }
    // }
    // We need both parser and semanticdb for synthetic decorations
    val publishSynthetics = {
      implicit val ec = pools.documentChangeEc
      val checks = for {
        targetId    <- bspData.inverseSources0(path).merge
        buildClient <- bspData.buildClientOf(targetId).toSeq
        dialect     <- bspData.getDialect(path.ext, path.isMill, targetId).toSeq
      } yield parserQueue.check(targetId.module, path, buildClient, dialect)
      val f = for {
        _ <- Future.sequence(checks ++ Seq(Future(interactive)))
        _ <- Future.sequence(
          List[Future[_]](
            // publishSynthetics0(path, server, cancelTokensEces, dummyEc)
            // testProvider.didOpen(path),
          )
        )
      } yield ()
      f.onComplete {
        case Success(()) =>
        case Failure(ex) =>
          scribe.warn(s"Error while publishing synthetics upon opening $path", ex)
      }
      f
    }

    if (!path.isDependencySource(workspace())) {
      implicit val ec = pools.documentChangeEc
      Future
        .sequence(
          List(
            presentationCompilers.load(List(path)),
            publishSynthetics
          ) ++
            compilations.compileFile(path).toSeq
        )
        .onComplete {
          case Success(_) =>
          case Failure(ex) =>
            scribe.error(s"Error loading $path", ex)
        }
    }

    SourcePath.withContext { implicit ctx =>
      new PackageProvider(bspData, editorState.trees)
        .workspaceEdit(
          path,
          currentContent,
          Some(contentVersion)
        )
        .map(new l.ApplyWorkspaceEditParams(_))
        .foreach(languageClient.applyEdit)
    }
  }

  def editorFileChanged(path: os.Path, updatedContent: String): Unit = {
    editorState.buffers.put(path, updatedContent)

    refreshStatus()
    for {
      targetId    <- bspData.inverseSources0(path).merge
      buildClient <- bspData.buildClientOf(targetId)
    } {
      buildClient.diagDidChange(targetId.module, path)

      for (dialect <- bspData.getDialect(path.ext, path.isMill, targetId))
        parserQueue
          .check(targetId.module, path, buildClient, dialect)
          .onComplete {
            case Success(()) =>
            case Failure(ex) => scribe.error(s"Error parsing $path", ex)
          }(pools.documentChangeEc)
    }
    //   .flatMap(_ => publishSynthetics0(path, server, cancelTokensEces, dummyEc))(
    //     pools.documentChangeEc
    //   )
    //   .ignoreValue(pools.documentChangeEc)
  }

  def editorFileSaved(path: os.Path): Unit = {
    refreshStatus()
    // savedFiles.add(path)
    // read file from disk, we only remove files from buffers on didClose.
    editorState.buffers.put(path, os.read(path))
    reindexSource(path)
    implicit val ec = pools.documentChangeEc
    val checks = for {
      targetId    <- bspData.inverseSources0(path).merge
      buildClient <- bspData.buildClientOf(targetId).toSeq
      dialect     <- bspData.getDialect(path.ext, path.isMill, targetId).toSeq
    } yield parserQueue.check(targetId.module, path, buildClient, dialect)
    Future
      .sequence(
        checks ++ List(
          compilations.compileFiles(Seq(path)),
          // onBuildChanged(paths).ignoreValue,
          // Future.sequence(paths.map(onBuildToolAdded)),
          bspData
            .inverseSources(path)
            .map { targetId =>
              Future(interactiveSemanticdbs.textDocument(
                path,
                targetId.module
              ))(pools.documentChangeEc)
            }
            .getOrElse(Future.successful(()))
        )
        // renameProvider.runSave(),
        // ++ // if we fixed the script, we might need to retry connection
        // maybeImportScript(
        //   path
        // )
      )
      .ignoreValue
      .onComplete {
        case Success(()) =>
        case Failure(ex) =>
          scribe.error(s"Error handling save of $path", ex)
      }
  }

  def editorFileClosed(path: os.Path): Unit = {
    editorState.closed(path)
    presentationCompilers.didClose(path)
    for {
      targetId    <- bspData.inverseSources0(path).merge
      buildClient <- bspData.buildClientOf(targetId)
    }
      buildClient.onClose(targetId.module, path)
  }

  private def fileChangedOrCreatedUpdateState(path: os.Path, created: Boolean): Unit = {
    if (created)
      editorState.fingerprints.add(path, os.read(path))
  }

  private def fileChangedOrCreatedInternal(path: os.Path, created: Boolean): Unit = {
    if (created)
      bspData.onCreate(path)

    // Future(indexer.reindexWorkspaceSources(List(path)))(???),
    // onBuildChanged(List(path)),
    bspData.inverseSources(path) match {
      case None =>
        scribe.warn(s"No build target found for changed file $path")
      case Some(targetId) =>
        compilations.compileTarget(targetId).onComplete {
          case Success(_) =>
          case Failure(ex) =>
            scribe.error(s"Error compiling $path upon change", ex)
        }(pools.dummyEc)

        Future(interactiveSemanticdbs.textDocument(
          path,
          targetId.module
        ))(pools.indexingEc)
          .onComplete {
            case Success(_) =>
            case Failure(ex) =>
              scribe.error(s"Error computing semanticdb with presentation compiler for $path", ex)
          }(pools.dummyEc)
    }
  }

  def fileChangedOrCreated(path: os.Path, created: Boolean): Unit = {
    fileChangedOrCreatedUpdateState(path, created)
    fileWatcher.enqueue(WatchEvent.CreateOrModify(path))
  }

  private def fileDeletedInternal(path: os.Path): Unit = {

    for {
      targetId    <- bspData.inverseSources0(path).merge
      buildClient <- bspData.buildClientOf(targetId)
    }
      buildClient.didDelete(path)

    compilations.compileFile(path) match {
      case None =>
        scribe.warn(s"No build target found for deleted file $path")
      case Some(f) =>
        f.onComplete {
          case Success(_) =>
          case Failure(ex) =>
            scribe.error(s"Error compiling $path upon deletion", ex)
        }(pools.dummyEc)
    }
    // testProvider.onFileDelete(path)
  }

  def fileDeleted(path: os.Path): Unit =
    fileWatcher.enqueue(WatchEvent.Delete(path))

  private def onNewSemanticdb(
    module: GlobalSymbolIndex.Module,
    path: os.Path,
    semdb: TextDocument
  ) =
    semanticDBIndexer.onChange(module, path, semdb)

  private def postCompilation(targetIds: Seq[b.BuildTargetIdentifier]): Unit = {
    // FIXME Do more
    refreshStatus()
    for (targetId <- targetIds)
      presentationCompilers.reset(targetId)
  }

  def readAllSemanticdbs(): Unit =
    semanticDBIndexer.onTargetRoots

  def resetCaches(): Unit = {

    // Ideally, during indexing, these shouldn't be accessed by LSP requests
    // (borked results)

    interactiveSemanticdbs.reset()
    semanticDBIndexer.reset()
    symbolIndex.clear()
    symbolDocs.reset()
    bspData.clearTargetData
    presentationCompilers.reset()

    jdkContext.reset()
    javaFileManager.resetCache()
  }
}

object Server {
  sealed abstract class WatchEvent extends Product with Serializable {
    def process(server: Server): Unit
  }

  object WatchEvent {
    sealed abstract class FileChange extends WatchEvent {
      def path: os.Path
    }
    final case class FileCreated(path: os.Path) extends FileChange {
      def process(server: Server): Unit =
        ???
    }
    final case class FileChanged(path: os.Path) extends FileChange {
      def process(server: Server): Unit =
        ???
    }
    final case class FileDeleted(path: os.Path) extends FileChange {
      def process(server: Server): Unit =
        ???
    }
    final case class BuildTargetEvents(events: Seq[b.BuildTargetEvent]) extends WatchEvent {
      def process(server: Server): Unit =
        ???
    }

    private def normalizeSamePathFileChanges(changes: Seq[FileChange]): Seq[FileChange] = {
      def helper(current: Option[FileChange], list: List[FileChange]): Option[FileChange] =
        list match {
          case Nil => current
          case h :: t =>
            h match {
              case _: FileCreated =>
                val current0 = current match {
                  case Some(_: FileDeleted) => FileChanged(h.path)
                  case _                    => h
                }
                helper(Some(current0), t)
              case _: FileChanged =>
                val current0 = current match {
                  case Some(_: FileCreated) => current
                  case _                    => Some(h)
                }
                helper(current0, t)
              case _: FileDeleted =>
                val current0 = current match {
                  case Some(_: FileCreated) => None
                  case _                    => Some(h)
                }
                helper(current0, t)
            }
        }

      helper(None, changes.toList).toSeq
    }

    def normalize(changes: Seq[WatchEvent]): Seq[WatchEvent] = {
      val allBuildTargetEvents = changes.collect {
        case e: BuildTargetEvents => e
      }
      val finalBuildTargetEvents =
        if (allBuildTargetEvents.lengthCompare(1) <= 0) allBuildTargetEvents
        else Seq(BuildTargetEvents(allBuildTargetEvents.flatMap(_.events)))
      val fileChanges = changes
        .zipWithIndex
        .collect {
          case (c: FileChange, idx) => (c, idx)
        }
        .groupBy(_._1.path)
        .flatMap {
          case (_, changes0) =>
            assert(changes0.nonEmpty)
            normalizeSamePathFileChanges(changes0.map(_._1)).map((_, changes0.head._2))
        }
        .toVector
        .sortBy(_._2)
        .map(_._1)
      fileChanges ++ finalBuildTargetEvents
    }
  }
}
