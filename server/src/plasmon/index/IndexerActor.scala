package plasmon.index

import org.eclipse.{lsp4j => l}
import ch.epfl.scala.{bsp4j => b}
import scala.meta.internal.mtags.SourcePath
import plasmon.bsp.BuildServerInfo
import java.time.Instant
import java.time.OffsetDateTime
import java.time.ZoneId
import plasmon.Logger
import scala.concurrent.Future
import scala.meta.internal.mtags.OnDemandSymbolIndex
import scala.util.Success
import scala.util.Failure
import plasmon.servercommand.BspUtil
import java.time.temporal.ChronoUnit
import scala.meta.internal.metals.JdkSources
import plasmon.HasState
import plasmon.bsp.PlasmonBuildServer
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.build.bsp.WrappedSourcesParams
import org.eclipse.lsp4j.jsonrpc.ResponseErrorException
import org.eclipse.lsp4j.jsonrpc.messages.ResponseErrorCode
import scala.build.bsp.WrappedSourcesResult
import scala.meta.inputs.Input
import scala.meta.internal.metals.ScalaVersions
import scala.util.control.NonFatal
import scala.collection.mutable.ArrayBuffer
import scala.meta.internal.metals.WorkspaceSymbolInformation
import scala.meta.internal.metals.SemanticdbDefinition
import java.util.zip.ZipFile
import java.net.URI
import scala.meta.tokenizers.TokenizeException
import scala.util.Properties
import scala.meta.Dialect
import scala.util.Try
import scala.concurrent.ExecutionContext
import java.util.concurrent.ExecutionException
import scala.annotation.nowarn
import plasmon.ide.{AdjustLspData, AdjustedLspData}
import plasmon.index.TargetData

import plasmon.PlasmonEnrichments._
import scala.jdk.CollectionConverters._
import plasmon.pc.NopReportContext
import scala.meta.internal.mtags.GlobalSymbolIndex
import coursier.version.Version
import plasmon.bsp.BspConnection
import scala.concurrent.Promise
import sourcecode.FileName
import sourcecode.Line
import com.google.gson.GsonBuilder
import scala.reflect.ClassTag

class IndexerActor(
  server: IndexerServerLike,
  logger: Logger
)(implicit ctx: castor.Context) extends plasmon.PlasmonActor[IndexerActor.Message]
    with HasState[String] {

  import IndexerActor.*

  var latestToplevelCacheOnly           = false
  var latestIgnoreToplevelSymbolsErrors = true
  var latestIndexed                     = Seq.empty[(BuildServerInfo, Seq[b.BuildTargetIdentifier])]

  def languageClient         = server.languageClient
  protected def progressId   = "indexer"
  protected def progressName = "Indexer"

  private def interruptIndexing(): Boolean =
    awaitingMessages.exists {
      case _: Message.Index => true
      case _                => false
    }
  private var interruptIndexingPromiseOpt = Option.empty[Promise[Unit]]
  private def resetInterruptIndexingPromise(): Unit = {
    interruptIndexingPromiseOpt.foreach(_.trySuccess(()))
    interruptIndexingPromiseOpt = None
  }

  override def send(t: Message)(implicit fileName: FileName, line: Line): Unit = {
    super.send(t)
    t match {
      case _: Message.Index =>
        interruptIndexingPromiseOpt.foreach(_.trySuccess(()))
      case Message.InterruptIndexing =>
        interruptIndexingPromiseOpt.foreach(_.trySuccess(()))
      case _ =>
    }
  }

  override def runBatch(msgs: Seq[Message]): Unit = {
    super.runBatch(msgs)

    val interruptIndexingIdxOpt = Some(msgs.lastIndexOf(Message.InterruptIndexing)).filter(_ >= 0)
    val indexMessages = msgs.zipWithIndex.collect {
      case (msg: Message.Index, idx) if interruptIndexingIdxOpt.forall(idx > _) =>
        (msg, idx)
    }
    val indexMessageOpt = indexMessages.headOption.map {
      case (first, _) =>
        (Message.Index.sum(first, indexMessages.tail.map(_._1)), indexMessages.last._2)
    }
    val nonIndexMessages = indexMessageOpt match {
      case None           => msgs
      case Some((_, idx)) => msgs.drop(idx + 1)
    }

    for ((indexMessage, _) <- indexMessageOpt)
      try
        if (!interruptIndexing()) {
          val res = Try {
            SourcePath.withContext { implicit ctx =>
              val res = doIndex(indexMessage, () => interruptIndexing())
              if (!interruptIndexing()) {
                latestToplevelCacheOnly =
                  indexMessage.toplevelCacheOnly.getOrElse(latestToplevelCacheOnly)
                latestIgnoreToplevelSymbolsErrors =
                  indexMessage.ignoreToplevelSymbolsErrors
                    .getOrElse(latestIgnoreToplevelSymbolsErrors)
                latestIndexed = res

                for (persistTo <- indexMessage.persistTo)
                  Persist.persistTargets(
                    indexMessage.targets,
                    persistTo
                  )
              }
            }
          }
          if (!interruptIndexing())
            indexMessage.onDone.foreach(_(res))
        }
      finally resetInterruptIndexingPromise()

    nonIndexMessages.foreach {
      case exp: Message.ExpireSymbolDocDefinitions =>
        val dialectOpt =
          if (exp.source.last.endsWith(".sbt")) Some(scala.meta.dialects.Sbt)
          else sourceDialect(exp.target, exp.data)

        doExpireSymbolDocDefinitions(exp.source, exp.target, dialectOpt)
      case idxSym: Message.IndexWorkspaceSourceSymbols =>
        doIndexWorkspaceSourceSymbols(
          idxSym.source,
          idxSym.sourceItem,
          idxSym.target,
          idxSym.data
        )
      case Message.InterruptIndexing =>
      // ignored
      case _: Message.Index =>
        sys.error("Cannot happen")
    }
  }

  private def doIndex(
    message: Message.Index,
    interruptIndexing: () => Boolean
  )(implicit ctx: SourcePath.Context): Seq[(BuildServerInfo, Seq[b.BuildTargetIdentifier])] = {

    if (interruptIndexing()) return Nil

    inState("Indexing", Some(logger), progress = "Indexing") {
      doIndex0(message, interruptIndexing)
    }
  }

  private def doIndex0(
    message: Message.Index,
    interruptIndexing: () => Boolean
  )(implicit ctx: SourcePath.Context): Seq[(BuildServerInfo, Seq[b.BuildTargetIdentifier])] = {

    if (interruptIndexing()) return Nil

    val startTime = Instant.now()

    logger.log(
      s"""
         |  Indexing starting...
         |  ${OffsetDateTime.ofInstant(startTime, ZoneId.systemDefault()).toLocalDateTime()}
         |""".stripMargin
    )

    if (interruptIndexing()) return Nil

    val toplevelSymbolsCache = new ToplevelSymbolsCache(
      server.workingDir / ".plasmon/cache/toplevel",
      os.Path(coursierapi.Cache.create().getLocation, os.pwd),
      os.Path(coursierapi.ArchiveCache.create().getLocation, os.pwd),
      readOnly = message.toplevelCacheOnly.getOrElse(latestToplevelCacheOnly)
    )

    if (interruptIndexing()) return Nil

    var stepCount = 0

    stepCount += 1
    inState(s"Indexing ($stepCount)", Some(logger), progress = "Resetting caches") {
      logger.timed("Resetting caches") {
        server.resetCaches()
      }
    }

    if (interruptIndexing()) return Nil

    for (path <- server.jdkSources) {
      stepCount += 1
      inState(
        s"Indexing ($stepCount)",
        Some(logger),
        progress = "Adding JDK source JARs to index"
      ) {
        logger.timed("Adding JDK source JARs to index") {
          server.symbolIndex.addSourceJar(path.toAbsPath)
        }
      }
    }

    if (interruptIndexing()) return Nil

    val buildServers     = server.bspServers.list
    val buildServerCount = buildServers.length
    val initialStepCount = stepCount
    val targetsPerBuildServer = buildServers.flatMap(_._2).zipWithIndex.flatMap {
      case (conn, connIdx) =>
        stepCount = initialStepCount
        def stateName(): String = {
          stepCount += 1
          if (buildServerCount <= 1)
            s"Indexing ($stepCount)"
          else
            s"Indexing ($stepCount, ${connIdx + 1} / $buildServerCount)"
        }

        indexBspServer(
          conn,
          stateName,
          message,
          toplevelSymbolsCache,
          server.workingDir / ".plasmon/cache/bsp",
          mayReadFromCache = message.mayReadFromBspCache,
          interruptIndexing
        ).toSeq
    }

    if (interruptIndexing()) return Nil

    stepCount += 1
    inState(
      s"Indexing ($stepCount)",
      Some(logger),
      progress = "Indexing JARs for fuzzy class search"
    ) {
      logger.timed("Indexing JARs for fuzzy class search") {
        server.symbolSearchIndex.indexClasspathUnsafe(
          Nil,
          server.jdkCp
        )
      }
    }

    if (interruptIndexing()) return Nil

    stepCount += 1
    inState(s"Indexing ($stepCount)", Some(logger), progress = "Updating semanticdb stuff") {
      logger.timed("Updating semanticdb stuff") {
        server.readAllSemanticdbs()
      }
    }

    if (interruptIndexing()) return Nil

    server.fileWatcher.start() // ensure the file watcher is running

    if (interruptIndexing()) return Nil

    // for {
    //   doc <- focusedDocument()
    //   id <- bspData.inverseSources(doc)
    // }
    //   focusedDocumentBuildTarget.set(id)

    // buildTargetClasses
    //   .rebuildIndex(targets)
    //   .map { _ =>
    //     languageClient.refreshModel()
    //   }
    //   .onComplete {
    //     case Success(_)  =>
    //     case Failure(ex) =>
    //       scribe.warn("Error rebuilding classes index", ex)
    //   }

    val endTime         = Instant.now()
    val durationSeconds = ChronoUnit.SECONDS.between(startTime, endTime)
    logger.log {
      val seconds = if (durationSeconds == 1) "second" else "seconds"
      s"""
         |  Indexing done, took $durationSeconds $seconds
         |
         |""".stripMargin
    }

    if (interruptIndexing()) return Nil

    val openedFiles = server.editorState.buffers.open.toVector
    if (openedFiles.nonEmpty) {
      logger.log(s"Compiling ${openedFiles.length} file(s)")
      server.compilations.compileFiles(openedFiles).onComplete {
        case Success(()) =>
          logger.log("Done compiling opened file(s)")
        case Failure(ex) =>
          logger.log(s"Compiling opened file(s) failed: $ex")
          scribe.error("Compiling opened file(s) failed", ex)
      }(using server.pools.compilationEc)
    }

    targetsPerBuildServer
  }

  private def indexBspServer(
    conn: BspConnection,
    stateName: () => String,
    message: Message.Index,
    toplevelSymbolsCache: ToplevelSymbolsCache,
    bspDataCache: os.Path,
    mayReadFromCache: Boolean,
    interruptIndexing: () => Boolean
  )(implicit ctx: SourcePath.Context): Option[(BuildServerInfo, Seq[b.BuildTargetIdentifier])] = {

    if (interruptIndexing()) return None

    val info        = conn.info
    val buildServer = conn.conn
    val targetData = server.bspData.targetData(info).getOrElse {
      sys.error(s"No target data found for build server $info")
    }
    targetData.reset()

    if (interruptIndexing()) return None

    val isMill = conn.params.getDisplayName == "mill-bsp"

    val cacheDirOpt = conn.launcher.info match {
      case m: BuildServerInfo.Mill =>
        if (m.workspace.startsWith(server.workingDir))
          Some((
            bspDataCache / "mill" / m.workspace.subRelativeTo(server.workingDir),
            mayReadFromCache
          ))
        else
          None
      case _ => None
    }

    val (targets, depSourcesRes) =
      inState(stateName(), Some(logger), progress = s"Fetching BSP data for ${conn.name}") {
        logger.timed(s"Fetching BSP data for ${conn.name}") {
          conn.info.workspace
          val (workspaceBuildTargetsResp, targets0, depSourcesRes0) = fetchBspData(
            message,
            buildServer,
            info,
            targetData,
            server.jdkCp.map(_.toNIO.toUri.toASCIIString).toList,
            millHack = isMill,
            cacheDirOpt = cacheDirOpt
          )
          targetData.resetConnections(
            targets0
              .map(_.getId)
              .map((_, info.workspace))
              .toList,
            buildServer,
            conn.client,
            workspaceBuildTargetsResp
          )
          (targets0, depSourcesRes0)
        }
      }

    if (interruptIndexing()) return None

    inState(stateName(), Some(logger), progress = "Patching BSP data cache") {
      logger.timed("Patching BSP data cache") {
        linkSourceFiles(targetData)

        for {
          path <- server.jdkSources
          if !interruptIndexing()
          target <- targets
          if !interruptIndexing()
        } targetData.addDependencySource(path, target.getId)

        if (!interruptIndexing())
          addDependencySources(targetData, depSourcesRes)
      }
    }

    if (interruptIndexing()) return None

    inState(stateName(), Some(logger)) {
      languageClient.reportProgress(progressId, progressName, "Adding module tree to index") {
        logger.timed("Adding module tree to index") {
          for (target <- targets)
            if (!interruptIndexing()) {
              // FIXME That's quite inefficient, we should be able to build the whole dep tree at once
              // rather than for each dependency here
              val deps =
                BspUtil.addTransitiveDependencies(Seq(target.getId), targets) - target.getId
              server.symbolIndex.addDependsOn(target.getId.module, deps.map(_.module))
            }
        }
      }
      val files =
        languageClient.reportProgress(progressId, progressName, "Listing workspace sources") {
          logger.timed("Listing workspace sources") {
            for {
              (sourceItem, targets) <- targetData.sourceItemsToBuildTarget.toVector
              target = targets.asScala.headOption.getOrElse(sys.error("cannot happen"))
              source <- {
                if (interruptIndexing())
                  Nil
                else if (os.isDir(sourceItem))
                  os.walk(sourceItem).toVector
                else if (os.isFile(sourceItem))
                  Seq(sourceItem)
                else
                  Nil
              }
              if source.isScalaOrJava
            } yield (sourceItem, target, source)
          }
        }
      val sourcesStr = if (files.length == 1) "source" else "sources"
      if (!interruptIndexing())
        languageClient.reportProgress(
          progressId,
          progressName,
          s"Indexing toplevel definitions of ${files.length} workspace $sourcesStr"
        ) {
          logger.timed(
            s"Indexing toplevel definitions of ${files.length} workspace $sourcesStr"
          ) {
            for (target <- targets)
              if (!interruptIndexing()) {
                val dialectOpt = sourceDialect(target.getId, Seq(targetData))
                logger.log {
                  val dialectStr = dialectOpt.fold("no dialect")(d => s"dialect $d")
                  s"Adding target ${target.getId.getUri} to symbol index with $dialectStr"
                }
                server.symbolIndex.addModule(
                  GlobalSymbolIndex.BuildTarget(target.getId.getUri),
                  dialectOpt
                )
              }
            if (files.nonEmpty && !interruptIndexing()) {
              val interruptIndexingPromise = Promise[Unit]()
              interruptIndexingPromiseOpt = Some(interruptIndexingPromise)
              val f = indexWorkspaceSources(files, targetData, interruptIndexingPromise.future)
              Await.result(f, Duration.Inf)
            }
          }
        }
    }

    if (interruptIndexing()) return None

    inState(stateName(), Some(logger)) {
      val dependencySources: Seq[(b.BuildTargetIdentifier, String)] =
        for {
          item      <- depSourcesRes.getItems.asScala.toVector
          sourceUri <- Option(item.getSources).map(_.asScala.toVector).getOrElse(Vector.empty)
        } yield (item.getTarget, sourceUri)

      if (dependencySources.nonEmpty && !interruptIndexing()) {
        val msg =
          s"Indexing toplevel definitions of ${dependencySources.length} dependency " +
            (if (dependencySources.length == 1) "source" else "sources")
        languageClient.reportProgress(progressId, progressName, msg) {
          logger.timed(msg) {
            val scalaTargetMap = targets
              .flatMap { target =>
                target.asScalaBuildTarget.toSeq.map((target.getId, _))
              }
              .toMap

            indexDependencySources(
              dependencySources,
              server.symbolIndex,
              scalaTargetMap,
              toplevelSymbolsCache,
              message.ignoreToplevelSymbolsErrors.getOrElse(latestIgnoreToplevelSymbolsErrors),
              interruptIndexing
            )
          }
        }
      }
    }

    Some((info, targets.map(_.getId)))
  }

  private def linkSourceFiles(data: TargetData): Unit =
    for {
      (sourceItem, targets) <- data.sourceItemsToBuildTarget
      if os.isDir(sourceItem)
      source <- os.walk(sourceItem)
      if source.isScalaOrJava
    }
      for (target <- targets.asScala)
        data.linkSourceFile(target, source)

  private def indexWorkspaceSources(
    files: Seq[(os.Path, b.BuildTargetIdentifier, os.Path)],
    data: TargetData,
    shouldStop: Future[Unit]
  ): Future[Unit] = {

    @volatile var shouldStop0 = false

    val futures = files.map {
      case (sourceItem, target, source) =>
        Future {
          if (!shouldStop0) {
            val dialectOpt =
              if (source.last.endsWith(".sbt")) Some(scala.meta.dialects.Sbt)
              else sourceDialect(target, Seq(data))
            // FIXME Factor that so that we compute it only once per target
            if (!shouldStop0)
              doExpireSymbolDocDefinitions(source, target, dialectOpt)
            if (!shouldStop0)
              doIndexWorkspaceSourceSymbols(source, Some(sourceItem), target, Seq(data))
          }
        }(using server.pools.indexingEc)
    }

    @nowarn
    implicit val ec: ExecutionContext = server.pools.indexingEc

    shouldStop.onComplete {
      case Success(_) =>
        shouldStop0 = true
      case _ =>
    }

    Future.sequence(futures).map(_ => ())
  }

  private def fetchBspData(
    message: Message.Index,
    buildServer: PlasmonBuildServer,
    info: BuildServerInfo,
    targetData: TargetData,
    jdkCp: List[String],
    millHack: Boolean,
    cacheDirOpt: Option[(os.Path, Boolean)]
  ): (b.WorkspaceBuildTargetsResult, Seq[b.BuildTarget], b.DependencySourcesResult) = {

    lazy val gson = new GsonBuilder().create()
    def maybeCached[T: ClassTag](name: String)(get: => T): T =
      cacheDirOpt match {
        case None => get
        case Some((cacheDir, mayRead)) =>
          val f = cacheDir / s"$name.json"
          if (mayRead && os.exists(f)) {
            logger.log(s"BSP cache: reading $f")
            val content = os.read(f)
            gson.fromJson(content, implicitly[ClassTag[T]].runtimeClass.asInstanceOf[Class[T]])
          }
          else {
            val res     = get
            val jsonStr = gson.toJson(res)
            logger.log(s"BSP cache: writing $f")
            os.write.over(f, jsonStr, createFolders = true)
            res
          }
      }

    val workspaceBuildTargetsResp = maybeCached("workspaceBuildTargets") {
      buildServer.workspaceBuildTargets.get()
    }
    val roots = message.targets.getOrElse(info, Nil)

    for ((cacheDir, _) <- cacheDirOpt) {
      val content = gson.toJson(roots.asJava)
      val f       = cacheDir / "roots.json"
      val currentContentOpt =
        if (os.exists(f)) Some(os.read(f))
        else None
      if (!currentContentOpt.contains(content)) {
        logger.log(s"BSP cache: writing $f")
        os.write.over(f, content)
      }
    }

    val targets0 = {
      val allTargets = workspaceBuildTargetsResp.getTargets.asScala.toSeq

      val roots = message.targets.getOrElse(info, Nil)
      val keep  = BspUtil.addTransitiveDependencies(roots, allTargets)

      val retained = allTargets
        .iterator
        .filter(target => keep.contains(target.getId))
        .toList

      val missing = {
        val retainedSet = retained.map(_.getId).toSet
        roots.filter(!retainedSet.contains(_))
      }

      if (missing.nonEmpty) {
        scribe.warn(
          s"Missing targets in $info: ${missing.toVector.map(_.getUri).sorted.mkString(", ")}"
        )
        scribe.warn(
          s"Fount targets in $info: ${allTargets.toVector.map(_.getId.getUri).sorted.mkString(", ")}"
        )
      }

      retained
    }

    val targetList = targets0.map(_.getId).asJava
    scribe.info(
      s"Final retained targets in $info: ${targets0.map(_.getId.getUri)}"
    )

    targetData.addWorkspaceBuildTargets(targets0)
    targetData.addScalacOptions(
      postProcessScalacOptionResult(
        maybeCached("buildTargetScalacOptions") {
          buildServer.buildTargetScalacOptions(new b.ScalacOptionsParams(targetList)).get()
        },
        jdkCp,
        millHack,
        info.workspace
      )
    )
    // sbt is a pile of … and just doesn't do anything when sent a request it doesn't support
    // it just stays idle and do nothing, and everything's stuck
    if (info.id != "sbt")
      targetData.addJavacOptions(
        postProcessJavacOptionResult(
          maybeCached("buildTargetJavacOptions") {
            buildServer.buildTargetJavacOptions(new b.JavacOptionsParams(targetList)).get()
          },
          jdkCp,
          millHack,
          info.workspace
        )
      )

    val wrappedSourcesRes =
      if (info.id == "sbt") new WrappedSourcesResult(Nil.asJava)
      else
        maybeCached("buildTargetWrappedSources") {
          try
            buildServer
              .buildTargetWrappedSources(new WrappedSourcesParams(targetList))
              .get()
          catch {
            case e: ExecutionException =>
              e.getCause match {
                case ex: ResponseErrorException
                    if ex.getResponseError.getCode == ResponseErrorCode.MethodNotFound.getValue =>
                  scribe.warn(s"wrappedSources method not supported by $info, ignoring it")
                  new WrappedSourcesResult(Nil.asJava)
                case ex: org.eclipse.lsp4j.jsonrpc.MessageIssueException =>
                  scribe.warn(s"Error parsing message '${ex.getRpcMessage}', ignoring it")
                  for (i <- ex.getIssues.asScala)
                    scribe.warn(s"${i.getText} (${i.getIssueCode})", i.getCause)
                  // throw e
                  new WrappedSourcesResult(Nil.asJava)
                case _ =>
                  throw e
              }
          }
        }

    val mappedSources =
      for {
        item       <- wrappedSourcesRes.getItems.asScala.toVector
        sourceItem <- item.getSources.asScala.toVector
      } yield {
        val userPath = sourceItem.getUri.osPathFromUri
        val mappedSource: TargetData.MappedSource =
          new TargetData.MappedSource {
            val generatedPath       = sourceItem.getGeneratedUri.osPathFromUri
            val topWrapperLineCount = sourceItem.getTopWrapper.count(_ == '\n')
            val toScala: l.Position => l.Position =
              scPos =>
                new l.Position(
                  topWrapperLineCount + scPos.getLine,
                  scPos.getCharacter
                )
            val fromScala: l.Position => l.Position =
              scalaPos =>
                new l.Position(
                  scalaPos.getLine - topWrapperLineCount,
                  scalaPos.getCharacter
                )

            def path = generatedPath
            def update(
              content: String
            ): (Input.VirtualFile, l.Position => l.Position, AdjustLspData) = {
              val adjustLspData = AdjustedLspData.create(
                generatedPath,
                userPath,
                fromScala
              )
              val actualContent =
                // FIXME Dirty hack, mark that in the BSP wrapped sources messages?
                if (generatedPath.last.endsWith(".mill"))
                  content.linesIterator.zip(content.linesWithSeparators)
                    .map {
                      case (line, lineWithSep) =>
                        if (line.startsWith("package "))
                          lineWithSep.drop(line.length)
                        else
                          lineWithSep
                    }
                    .mkString
                else
                  content
              val updatedContent =
                sourceItem.getTopWrapper + actualContent + sourceItem.getBottomWrapper

              (
                Input.VirtualFile(
                  if (
                    generatedPath.last.endsWith(".scala") &&
                    !generatedPath.last.endsWith(".sc.scala")
                  )
                    generatedPath.toString.stripSuffix(".scala") + ".sc.scala"
                  else
                    generatedPath.toString,
                  updatedContent
                ),
                toScala,
                adjustLspData
              )
            }
            override def lineForServer(line: Int): Option[Int] =
              Some(line + topWrapperLineCount)
            override def lineForClient(line: Int): Option[Int] =
              Some(line - topWrapperLineCount).filter(_ >= 0)
          }
        (item.getTarget, userPath, mappedSource)
      }

    for ((targetId, path, mappedSource) <- mappedSources)
      targetData.addMappedSource(targetId, path, mappedSource)

    val sourcesRes = maybeCached("buildTargetSources") {
      buildServer.buildTargetSources(new b.SourcesParams(targetList)).get()
    }

    for {
      item       <- sourcesRes.getItems.asScala
      sourceItem <- item.getSources.asScala
    }
      targetData.addSourceItem(sourceItem, item.getTarget)

    val depSourcesRes = maybeCached("buildTargetDependencySources") {
      buildServer
        .buildTargetDependencySources(new b.DependencySourcesParams(targetList))
        .get()
    }

    (workspaceBuildTargetsResp, targets0, depSourcesRes)
  }

  private def addDependencySources(
    data: TargetData,
    dependencySources: b.DependencySourcesResult
  ): Unit =
    // Track used Jars so that we can
    // remove cached symbols from Jars
    // that are not used
    for {
      item <- dependencySources.getItems.asScala
      _ = data.addDependencySourceItem(item)
      sourceUri <- Option(item.getSources).toList.flatMap(_.asScala)
    }
      data.addDependencySource(sourceUri.osPathFromUri, item.getTarget)

  private def indexDependencySources(
    dependencySources: Seq[(b.BuildTargetIdentifier, String)],
    symbolIndex: OnDemandSymbolIndex,
    scalaTargetMap: Map[b.BuildTargetIdentifier, b.ScalaBuildTarget],
    toplevelSymbolsCache: ToplevelSymbolsCache,
    ignoreToplevelSymbolsErrors: Boolean,
    interruptIndexing: () => Boolean
  )(implicit ctx: SourcePath.Context): Unit = {
    import scala.meta.dialects.Scala213
    for ((targetId, sourceUri) <- dependencySources)
      if (!interruptIndexing())
        try {
          val path = sourceUri.osPathFromUri
          val svOpt = scalaTargetMap.get(targetId).map(_.getScalaVersion)
            .orElse {
              ScalaVersions.scalaBinaryVersionFromJarName(path.toNIO.getFileName.toString)
            }
          val dialectOpt = svOpt.map { sv =>
            ScalaVersions.dialectForScalaVersion(
              sv,
              includeSource3 = true
            )
          }
          if (path.isJar)
            addSourceJarSymbols(
              targetId,
              path,
              symbolIndex,
              toplevelSymbolsCache,
              dialectOpt,
              ignoreErrors = ignoreToplevelSymbolsErrors
            )
          else if (os.isDir(path))
            symbolIndex.addSourceDirectory(targetId.module, path.toAbsPath, dialectOpt)
          else
            logger.log(s"unexpected dependency (not a directory nor a JAR): $path")
        }
        catch {
          case e: ToplevelSymbolsCache.ToplevelSymbolsCacheException =>
            throw new Exception(s"Error processing $sourceUri", e)
          case NonFatal(e) if ignoreToplevelSymbolsErrors =>
            logger.log(s"error processing $sourceUri: $e")
            scribe.error(s"error processing $sourceUri", e)
        }
  }

  private def doExpireSymbolDocDefinitions(
    source: os.Path,
    target: b.BuildTargetIdentifier,
    dialectOpt: Option[Dialect]
  ): Unit =
    try {
      val sourceToIndex0 = server.bspData.mappedTo(target, source).map(_.path).getOrElse(source)
      if (os.exists(sourceToIndex0))
        // Since the `symbols` here are toplevel symbols,
        // we cannot use `symbols` for expiring the cache for all symbols in the source.
        server.symbolDocs.expireSymbolDefinition(
          target.module,
          sourceToIndex0.toAbsPath,
          dialectOpt
        )
    }
    catch {
      case NonFatal(e) =>
        scribe.error(source.toString(), e)
    }

  private def targetScalaVersionOpt(
    target: b.BuildTargetIdentifier,
    data: Seq[TargetData]
  ): Option[String] =
    data.iterator
      .flatMap(_.buildTargetInfo.get(target).iterator)
      .find(_ => true)
      .flatMap(_.asScalaBuildTarget)
      .map(_.getScalaVersion)

  private def sourceDialect(
    target: b.BuildTargetIdentifier,
    data: Seq[TargetData]
  ): Option[Dialect] =
    targetScalaVersionOpt(target, data).map { scalaVersion =>
      ScalaVersions.dialectForScalaVersion(
        scalaVersion,
        includeSource3 = true
      )
    }

  private def doIndexWorkspaceSourceSymbols(
    source: os.Path,
    sourceItem: Option[os.Path],
    target: b.BuildTargetIdentifier,
    data: Seq[TargetData]
  ): Unit =
    try {
      import scala.meta.internal.semanticdb.Scala.*
      val sourceToIndex0 = server.bspData.mappedTo(target, source).map(_.path).getOrElse(source)
      if (os.exists(sourceToIndex0)) {
        val dialectOpt    = sourceDialect(target, data)
        val reluri        = source.toIdeallyRelativeURI(sourceItem)
        val input         = sourceToIndex0.toInput
        val symbols       = ArrayBuffer.empty[WorkspaceSymbolInformation]
        val methodSymbols = ArrayBuffer.empty[WorkspaceSymbolInformation]
        SemanticdbDefinition.foreach(
          input,
          dialectOpt,
          includeMembers = true,
          logger = logger.log(_)
        ) {
          case SemanticdbDefinition(info, occ, owner) =>
            if (info.isExtension)
              occ.range.foreach { range =>
                methodSymbols += WorkspaceSymbolInformation(
                  info.symbol,
                  info.kind,
                  range.toLsp
                )
              }
            else if (info.kind.isRelevantKind)
              occ.range.foreach { range =>
                symbols += WorkspaceSymbolInformation(
                  info.symbol,
                  info.kind,
                  range.toLsp
                )
              }
            if (
              sourceItem.isDefined &&
              !info.symbol.isPackage &&
              (owner.isPackage || source.isScalaScript)
            )
              server.symbolIndex.addToplevelSymbol(
                target.module,
                reluri,
                SourcePath.Standard(source.toNIO),
                info.symbol,
                dialectOpt
              )
        }(using NopReportContext)
        server.symbolSearchIndex.didChange(source, symbols.toSeq, methodSymbols.toSeq)
      }
    }
    catch {
      case NonFatal(e) =>
        scribe.error(source.toString(), e)
    }

  private def addSourceJarSymbols(
    targetId: b.BuildTargetIdentifier,
    path: os.Path,
    symbolIndex: OnDemandSymbolIndex,
    toplevelSymbolsCache: ToplevelSymbolsCache,
    dialectOpt: Option[Dialect],
    ignoreErrors: Boolean
  )(implicit ctx: SourcePath.Context): Unit = {

    def compute()(implicit ctx: SourcePath.Context): Seq[(String, os.SubPath)] = {
      var zf: ZipFile = null
      try {
        zf = new ZipFile(path.toIO)
        zf.entries()
          .asScala
          .iterator
          .flatMap { entry =>
            if (entry.getName.endsWith(".scala") || entry.getName.endsWith(".java")) {
              val sourcePath = SourcePath.ZipEntry(path.toNIO, entry.getName, entry.getTime)
              val subPath    = os.sub / entry.getName.split("/").toSeq
              val input      = sourcePath.toInput
              try
                symbolIndex
                  .indexSource(targetId.module, input, dialectOpt)
                  .topLevels
                  .iterator
                  .map(_ -> subPath)
              catch {
                case e: TokenizeException =>
                  throw new Exception(
                    s"Error parsing ${sourcePath.uri} with dialect $dialectOpt",
                    e
                  )
              }
            }
            else
              Nil
          }
          .toVector
      }
      catch {
        case e: java.util.zip.ZipException if ignoreErrors =>
          logger.log(s"Ignoring error when reading source JAR $path: $e")
          scribe.error(s"Ignoring error when reading source JAR $path", e)
          Vector.empty[(String, os.SubPath)]
      }
      finally
        if (zf != null)
          zf.close()
    }

    val viaCacheOpt = toplevelSymbolsCache.get(
      os.Path(path.toNIO, os.pwd),
      () =>
        compute()
          .groupBy(_._1)
          .map { case (key, values) => key -> values.map(_._2) }
    )
    val toplevels =
      try
        viaCacheOpt
          .map(_.toVector.flatMap { case (key, values) => values.map(key -> _) })
          .getOrElse(compute())
          .toList
          .map {
            case (sym, subPath) =>
              (sym, SourcePath.ZipEntry(path.toNIO, subPath.segments.mkString("/"), -1))
          }
      catch {
        case e: java.util.zip.ZipException =>
          logger.log(s"Ignoring error when reading source JAR $path: $e")
          scribe.error(s"Ignoring error when reading source JAR $path", e)
          Nil
      }

    try symbolIndex.addIndexedSourceJar(targetId.module, path.toAbsPath, toplevels, dialectOpt)
    catch {
      case e: java.util.zip.ZipException =>
        logger.log(s"Ignoring error when reading source JAR $path: $e")
        scribe.error(s"Ignoring error when reading source JAR $path", e)
    }
  }

  private def jarOf(org: String, name: String, ver: String) = {
    val files = coursierapi.Fetch.create()
      .addDependencies(coursierapi.Dependency.of(org, name, ver).withTransitive(false))
      .fetch()
      .asScala
      .toList
    assert(files.length == 1)
    files.head.toPath.toUri.toASCIIString
  }
  private lazy val scalaLibraryJar =
    jarOf("org.scala-lang", "scala-library", Properties.versionNumberString)

  private def classPathMillHack(
    classDir: String,
    classPath: List[String],
    workspace: os.Path
  ): Option[List[String]] = {
    val usesCompiledClassesAndSemanticDbFilesDest = {
      val classDir0 = classDir.osPathFromUri
      classDir0.startsWith(workspace) &&
      classDir0.subRelativeTo(workspace).segments.contains("compiledClassesAndSemanticDbFiles.dest")
    }
    if (usesCompiledClassesAndSemanticDbFilesDest) {
      var didUpdateClasspath = false
      val maybeUpdatedClasspath = classPath.map { uri =>
        var uri0 = uri
        val path = uri.osPathFromUri
        if (path.startsWith(workspace)) {
          val subPath = path.subRelativeTo(workspace)
          if (subPath.endsWith(os.rel / "compile.dest/classes") && !os.exists(path)) {
            val updatedSubPath = subPath / os.up / os.up / "compiledClassesAndSemanticDbFiles.dest"
            val updatedPath    = workspace / updatedSubPath
            didUpdateClasspath = true
            uri0 = updatedPath.toNIO.toUri.toASCIIString
          }
        }
        uri0
      }
      if (didUpdateClasspath) Some(maybeUpdatedClasspath)
      else None
    }
    else
      None
  }

  private lazy val substitutableVersionCutoff = Version("3.8.0-RC1")
  private def isSubstitutableScalaLibrary(fileName: String): Boolean =
    fileName.startsWith("scala-library-") &&
    fileName.endsWith(".jar")
  private def isScala3Library3(fileName: String): Boolean =
    fileName.endsWith(".jar") && {
      fileName.startsWith("scala3-library_3-") ||
      fileName.startsWith("scala3-library_sjs1_3-")
    }

  private def postProcessScalacOptionResult(
    res: b.ScalacOptionsResult,
    extraCp: List[String],
    millHack: Boolean,
    workspace: os.Path
  ): b.ScalacOptionsResult = {
    for (item <- res.getItems.asScala.toList) {
      var didUpdateClasspath = false
      var updatedClasspath = item.getClasspath.asScala.toList.flatMap { elem =>
        val path = elem.osPathFromUri
        if (isSubstitutableScalaLibrary(path.last)) {
          didUpdateClasspath = true
          Seq(scalaLibraryJar)
        }
        else if (isScala3Library3(path.last)) {
          didUpdateClasspath = true
          // Removing scala3-library_3
          Nil
        }
        else
          Seq(elem)
      }
      if (millHack)
        for (cp <- classPathMillHack(item.getClassDirectory, updatedClasspath, workspace)) {
          didUpdateClasspath = true
          updatedClasspath = cp
        }
      if (extraCp.nonEmpty) {
        didUpdateClasspath = true
        updatedClasspath = updatedClasspath ::: extraCp
      }
      if (didUpdateClasspath)
        item.setClasspath(updatedClasspath.asJava)
    }
    res
  }

  private def postProcessJavacOptionResult(
    res: b.JavacOptionsResult,
    extraCp: List[String],
    millHack: Boolean,
    workspace: os.Path
  ): b.JavacOptionsResult = {
    for (item <- res.getItems.asScala.toList) {
      var didUpdateClasspath = false
      var updatedClasspath   = item.getClasspath.asScala.toList
      if (millHack)
        for (cp <- classPathMillHack(item.getClassDirectory, updatedClasspath, workspace)) {
          didUpdateClasspath = true
          updatedClasspath = cp
        }
      if (extraCp.nonEmpty) {
        didUpdateClasspath = true
        updatedClasspath = updatedClasspath ::: extraCp
      }
      if (didUpdateClasspath)
        item.setClasspath(updatedClasspath.asJava)
    }
    res
  }
}

object IndexerActor {
  sealed abstract class Message extends Product with Serializable
  object Message {
    case object InterruptIndexing extends Message
    final case class Index(
      targets: Map[BuildServerInfo, Seq[b.BuildTargetIdentifier]],
      toplevelCacheOnly: Option[Boolean],
      ignoreToplevelSymbolsErrors: Option[Boolean],
      persistTo: Option[os.Path],
      onDone: Option[Try[Unit] => Unit],
      mayReadFromBspCache: Boolean
    ) extends Message
    object Index {
      def sum(first: Index, others: Seq[Index]): Index = {
        var main      = first
        var persistTo = first.persistTo
        var onDone    = first.onDone
        for (msg <- others) {
          main = msg
          persistTo = msg.persistTo.orElse(persistTo)
          for (f <- msg.onDone)
            onDone = Some {
              onDone match {
                case Some(onDone0) =>
                  res => {
                    onDone0(res)
                    f(res)
                  }
                case None => f
              }
            }
        }
        main.copy(persistTo = persistTo, onDone = onDone)
      }
    }
    final case class ExpireSymbolDocDefinitions(
      source: os.Path,
      target: b.BuildTargetIdentifier,
      data: Seq[TargetData]
    ) extends Message
    final case class IndexWorkspaceSourceSymbols(
      source: os.Path,
      sourceItem: Option[os.Path],
      target: b.BuildTargetIdentifier,
      data: Seq[TargetData]
    ) extends Message
  }
}
