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

  override def runBatch(msgs: Seq[Message]): Unit = {
    super.runBatch(msgs)
    val indexMessages = msgs.zipWithIndex.collect {
      case (msg: Message.Index, idx) =>
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
    for ((indexMessage, _) <- indexMessageOpt) {
      val res = Try {
        SourcePath.withContext { implicit ctx =>
          val res = doIndex(indexMessage)
          latestToplevelCacheOnly =
            indexMessage.toplevelCacheOnly.getOrElse(latestToplevelCacheOnly)
          latestIgnoreToplevelSymbolsErrors =
            indexMessage.ignoreToplevelSymbolsErrors.getOrElse(latestIgnoreToplevelSymbolsErrors)
          latestIndexed = res
          for (persistTo <- indexMessage.persistTo)
            Persist.persistTargets(
              indexMessage.addAllTargets,
              indexMessage.targets,
              persistTo
            )
        }
      }
      indexMessage.onDone.foreach(_(res))
    }
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
      case _: Message.Index =>
        sys.error("Cannot happen")
    }
  }

  private def doIndex(
    message: Message.Index
  )(implicit ctx: SourcePath.Context): Seq[(BuildServerInfo, Seq[b.BuildTargetIdentifier])] = {
    val startTime = Instant.now()
    inState("Indexing", Some(logger), progress = "Indexing") {
      logger.log(
        s"""
           |  Indexing starting...
           |  ${OffsetDateTime.ofInstant(startTime, ZoneId.systemDefault()).toLocalDateTime()}
           |""".stripMargin
      )

      val toplevelSymbolsCache = new ToplevelSymbolsCache(
        server.workingDir / ".plasmon/toplevel-cache",
        os.Path(coursierapi.Cache.create().getLocation, os.pwd),
        os.Path(coursierapi.ArchiveCache.create().getLocation, os.pwd),
        readOnly = message.toplevelCacheOnly.getOrElse(latestToplevelCacheOnly)
      )

      var stepCount = 0

      stepCount += 1
      inState(s"Indexing ($stepCount)", Some(logger), progress = "Resetting caches") {
        logger.timed("Resetting caches") {
          server.resetCaches()
        }
      }

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

      val buildServers     = server.bspServers.list
      val buildServerCount = buildServers.length
      val initialStepCount = stepCount
      val targetsPerBuildServer = buildServers.flatMap(_._2).zipWithIndex.map {
        case (conn, connIdx) =>
          stepCount = initialStepCount
          def stateName(): String = {
            stepCount += 1
            if (buildServerCount <= 1)
              s"Indexing ($stepCount)"
            else
              s"Indexing ($stepCount, ${connIdx + 1} / $buildServerCount)"
          }
          val info        = conn.info
          val buildServer = conn.conn
          val targetData = server.bspData.targetData(info).getOrElse {
            sys.error(s"No target data found for build server $info")
          }
          targetData.reset()

          val (targets, depSourcesRes) =
            inState(stateName(), Some(logger), progress = s"Fetching BSP data for ${conn.name}") {
              logger.timed(s"Fetching BSP data for ${conn.name}") {
                val (workspaceBuildTargetsResp, targets0, depSourcesRes0) = fetchBspData(
                  message,
                  buildServer,
                  info,
                  targetData,
                  server.jdkCp.map(_.toNIO.toUri.toASCIIString).toList
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

          inState(stateName(), Some(logger), progress = "Patching BSP data cache") {
            logger.timed("Patching BSP data cache") {
              linkSourceFiles(targetData)

              for {
                path   <- server.jdkSources
                target <- targets
              } targetData.addDependencySource(path, target.getId)

              addDependencySources(targetData, depSourcesRes)
            }
          }

          inState(stateName(), Some(logger)) {
            languageClient.reportProgress(progressId, progressName, "Adding module tree to index") {
              logger.timed("Adding module tree to index") {
                for (target <- targets) {
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
                      if (os.isDir(sourceItem))
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
            languageClient.reportProgress(
              progressId,
              progressName,
              s"Indexing toplevel definitions of ${files.length} workspace $sourcesStr"
            ) {
              logger.timed(
                s"Indexing toplevel definitions of ${files.length} workspace $sourcesStr"
              ) {
                for (target <- targets) {
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
                if (files.nonEmpty) {
                  val f = indexWorkspaceSources(files, targetData)
                  Await.result(f, Duration.Inf)
                }
              }
            }
          }

          inState(stateName(), Some(logger)) {
            val dependencySources: Seq[(b.BuildTargetIdentifier, String)] =
              for {
                item      <- depSourcesRes.getItems.asScala.toVector
                sourceUri <- Option(item.getSources).map(_.asScala.toVector).getOrElse(Vector.empty)
              } yield (item.getTarget, sourceUri)

            if (dependencySources.nonEmpty) {
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
                    message.ignoreToplevelSymbolsErrors.getOrElse(latestIgnoreToplevelSymbolsErrors)
                  )
                }
              }
            }
          }

          (info, targets.map(_.getId))
      }

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

      stepCount += 1
      inState(s"Indexing ($stepCount)", Some(logger), progress = "Updating semanticdb stuff") {
        logger.timed("Updating semanticdb stuff") {
          server.readAllSemanticdbs()
        }
      }

      server.fileWatcher.start() // ensure the file watcher is running

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
      logger.log(
        s"""
           |  Indexing done, took $durationSeconds ${if (durationSeconds == 1) "second"
          else "seconds"}
           |
           |""".stripMargin
      )

      val openedFiles = server.editorState.buffers.open.toVector
      if (openedFiles.nonEmpty) {
        logger.log(s"Compiling ${openedFiles.length} file(s)")
        server.compilations.compileFiles(openedFiles).onComplete {
          case Success(()) =>
            logger.log("Done compiling opened file(s)")
          case Failure(ex) =>
            logger.log(s"Compiling opened file(s) failed: $ex")
            scribe.error("Compiling opened file(s) failed", ex)
        }(server.pools.compilationEc)
      }

      targetsPerBuildServer
    }
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
    data: TargetData
  ): Future[Unit] = {

    val futures = files.map {
      case (sourceItem, target, source) =>
        Future {
          val dialectOpt =
            if (source.last.endsWith(".sbt")) Some(scala.meta.dialects.Sbt)
            else sourceDialect(target, Seq(data))
          // FIXME Factor that so that we compute it only once per target
          doExpireSymbolDocDefinitions(source, target, dialectOpt)
          doIndexWorkspaceSourceSymbols(source, Some(sourceItem), target, Seq(data))
        }(server.pools.indexingEc)
    }

    @nowarn
    implicit val ec: ExecutionContext = server.pools.indexingEc
    Future.sequence(futures).map(_ => ())
  }

  private def fetchBspData(
    message: Message.Index,
    buildServer: PlasmonBuildServer,
    info: BuildServerInfo,
    targetData: TargetData,
    jdkCp: List[String]
  ): (b.WorkspaceBuildTargetsResult, Seq[b.BuildTarget], b.DependencySourcesResult) = {

    val workspaceBuildTargetsResp    = buildServer.workspaceBuildTargets.get()
    var targets0: Seq[b.BuildTarget] = workspaceBuildTargetsResp.getTargets.asScala.toSeq

    for (onlyTargets <- info.onlyTargets) {
      val (retained, dropped) = targets0.partition { target =>
        onlyTargets.contains(target.getId.getUri)
      }
      scribe.info(s"Dropped targets from $info: ${dropped.map(_.getId.getUri).sorted}")
      scribe.info(s"Retained targets from $info: ${retained.map(_.getId.getUri).sorted}")
      targets0 = retained
    }

    if (message.addAllTargets.contains(info))
      scribe.info(s"Keeping all targets for $info")
    else {

      val roots = message.targets.getOrElse(info, Nil)

      val keep = BspUtil.addTransitiveDependencies(roots, targets0)

      val retained = targets0
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
          s"Fount targets in $info: ${targets0.toVector.map(_.getId.getUri).sorted.mkString(", ")}"
        )
      }

      targets0 = retained
    }

    val targetList = targets0.map(_.getId).asJava
    scribe.info(
      s"Final retained targets in $info: ${targets0.map(_.getId.getUri)}"
    )

    targetData.addWorkspaceBuildTargets(targets0)
    targetData.addScalacOptions(
      postProcessScalacOptionResult(
        buildServer.buildTargetScalacOptions(new b.ScalacOptionsParams(targetList)).get(),
        jdkCp
      )
    )
    // sbt is a pile of … and just doesn't do anything when sent a request it doesn't support
    // it just stays idle and do nothing, and everything's stuck
    if (info.id != "sbt")
      targetData.addJavacOptions(
        postProcessJavacOptionResult(
          buildServer.buildTargetJavacOptions(new b.JavacOptionsParams(targetList)).get(),
          jdkCp
        )
      )

    val wrappedSourcesRes =
      if (info.id == "sbt") new WrappedSourcesResult(Nil.asJava)
      else
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

    val mappedSources =
      for {
        item       <- wrappedSourcesRes.getItems.asScala.toVector
        sourceItem <- item.getSources.asScala.toVector
      } yield {
        val path = sourceItem.getUri.osPathFromUri
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
              val adjustLspData = AdjustedLspData.create(fromScala)
              val updatedContent =
                sourceItem.getTopWrapper + content + sourceItem.getBottomWrapper
              (
                Input.VirtualFile(
                  generatedPath.toNIO.toString
                    .stripSuffix(".scala") + ".sc.scala",
                  updatedContent
                ),
                toScala,
                adjustLspData
              )
            }
            override def lineForServer(line: Int): Option[Int] =
              Some(line + topWrapperLineCount)
            override def lineForClient(line: Int): Option[Int] =
              Some(line - topWrapperLineCount)
          }
        (path, mappedSource)
      }

    for ((path, mappedSource) <- mappedSources)
      targetData.addMappedSource(path, mappedSource)

    val sourcesRes = buildServer.buildTargetSources(new b.SourcesParams(targetList)).get()

    for {
      item       <- sourcesRes.getItems.asScala
      sourceItem <- item.getSources.asScala
    }
      targetData.addSourceItem(sourceItem, item.getTarget)

    val depSourcesRes = buildServer
      .buildTargetDependencySources(new b.DependencySourcesParams(targetList))
      .get()

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
      item      <- dependencySources.getItems.asScala
      sourceUri <- Option(item.getSources).toList.flatMap(_.asScala)
    }
      data.addDependencySource(sourceUri.osPathFromUri, item.getTarget)

  private def indexDependencySources(
    dependencySources: Seq[(b.BuildTargetIdentifier, String)],
    symbolIndex: OnDemandSymbolIndex,
    scalaTargetMap: Map[b.BuildTargetIdentifier, b.ScalaBuildTarget],
    toplevelSymbolsCache: ToplevelSymbolsCache,
    ignoreToplevelSymbolsErrors: Boolean
  )(implicit ctx: SourcePath.Context): Unit = {
    import scala.meta.dialects.Scala213
    for ((targetId, sourceUri) <- dependencySources)
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
      val sourceToIndex0 = server.bspData.mappedTo(source).map(_.path).getOrElse(source)
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
      val sourceToIndex0 = server.bspData.mappedTo(source).map(_.path).getOrElse(source)
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
        }(NopReportContext)
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
  private lazy val scalaLibrary213 =
    jarOf("org.scala-lang", "scala-library", Properties.versionNumberString)
  private lazy val scalaReflect213 =
    jarOf("org.scala-lang", "scala-reflect", Properties.versionNumberString)

  private def postProcessScalacOptionResult(
    res: b.ScalacOptionsResult,
    extraCp: List[String]
  ): b.ScalacOptionsResult = {
    for (item <- res.getItems.asScala.toList) {
      var didUpdateClasspath = false
      var updatedClasspath = item.getClasspath.asScala.toList.map { elem =>
        val path = elem.osPathFromUri
        if (path.last.startsWith("scala-library-2.13.") && path.last.endsWith(".jar")) {
          didUpdateClasspath = true
          scalaLibrary213
        }
        else if (path.last.startsWith("scala-reflect-2.13.") && path.last.endsWith(".jar")) {
          didUpdateClasspath = true
          scalaReflect213
        }
        else
          elem
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
    extraCp: List[String]
  ): b.JavacOptionsResult = {
    if (extraCp.nonEmpty)
      for (item <- res.getItems.asScala.toList) {
        val updatedClasspath = item.getClasspath.asScala.toList ::: extraCp
        item.setClasspath(updatedClasspath.asJava)
      }
    res
  }
}

object IndexerActor {
  sealed abstract class Message extends Product with Serializable
  object Message {
    final case class Index(
      targets: Map[BuildServerInfo, Seq[b.BuildTargetIdentifier]],
      addAllTargets: Set[BuildServerInfo],
      toplevelCacheOnly: Option[Boolean],
      ignoreToplevelSymbolsErrors: Option[Boolean],
      persistTo: Option[os.Path],
      onDone: Option[Try[Unit] => Unit]
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
