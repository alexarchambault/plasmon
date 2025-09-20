package plasmon.servercommand

import caseapp.core.RemainingArgs
import ch.epfl.scala.{bsp4j => b}
import plasmon.command.ServerCommandThreadPools
import plasmon.protocol.CommandClient
import plasmon.util.PrintDiagnostic
import plasmon.index.Indexer
import plasmon.Server

import java.time.ZoneId

import scala.jdk.CollectionConverters.*
import plasmon.ide.Directories
import scala.meta.internal.pc.HasCompilerAccess
import plasmon.index.TargetData
import plasmon.pc.PresentationCompilers

final case class Inspect(
  server: Server,
  indexer: Indexer,
  client: CommandClient,
  lspServer: plasmon.jsonrpc.JsonrpcServer
) extends ServerCommandInstance[InspectOptions](client) {
  import Inspect.*
  def run(options: InspectOptions, args: RemainingArgs): Unit = {

    lazy val workingDir = options.workspace
      .filter(_.trim.nonEmpty)
      .map(os.Path(_, server.workingDir))
      .getOrElse(server.workingDir)

    if (options.list) {
      printLine("  Available items for inspection:")
      printLine(Inspect.CompilerCache)
      printLine(Inspect.OpenedFiles)
      printLine(Inspect.Opened)
      // printLine(Inspect.ClassPathIndex)
      printLine(Inspect.LoadedTargets)
      printLine(Inspect.Queues)
      for {
        targetData <- server.bspData.allTargetData
        targetId   <- targetData.buildTargetInfo.keys.toVector.sortBy(_.getUri)
      } {
        val printId =
          if (options.listUseShortIds)
            BspUtil.targetShortId(server.bspData, targetId)
          else
            targetId.getUri
        printLine(Inspect.BuildTargetData.prefix + printId)
        printLine(Inspect.Symbols.prefix + printId)
      }
    }

    args.all.foreach {
      case Inspect.CompilerCache =>
        val presentationCompilers = server
          .presentationCompilers
          .jcache
          .asScala
          .toVector
          .map {
            case (key: PresentationCompilers.PresentationCompilerKey.ScalaBuildTarget, c) =>
              (key.id.getUri, c)
            case (_: PresentationCompilers.PresentationCompilerKey.JavaBuildTarget, _) =>
              ???
          }
          .sortBy(_._1)
        for ((key, compiler) <- presentationCompilers) {
          val isLoaded = if (??? /*compiler.isLoaded()*/ ) "loaded" else "not loaded"
          val sv       = ??? : String // compiler.scalaVersion()
          val msg      = s"$key ($sv, $isLoaded)"
          val extra =
            if (options.verbose) System.lineSeparator() + pprint.apply(compiler).render
            else ""
          printLine(msg + extra)

          if (options.compilations)
            ???
          // val runningOpt = compiler.compilerAccess.jobs.runningJob()
          // val queue      = compiler.compilerAccess.jobs.jobQueue()
          // for (job <- runningOpt)
          //   printLine(s"  ${job.name} ${job.uri} (running)")
          // for (job <- queue)
          //   printLine(s"  ${job.name} ${job.uri}")
        }
      case BuildTargetData(maybeShortId) =>
        val targetId = BspUtil.targetFullId(workingDir, maybeShortId)
        printLine(s"Target ${targetId.getUri}")
        def targetDataElem[T](f: TargetData => Option[T]): Option[T] =
          server.bspData.allTargetData.iterator.flatMap(f(_).iterator).find(_ => true)
        val infoOpt                = targetDataElem(_.buildTargetInfo.get(targetId))
        val javaInfoOpt            = targetDataElem(_.javaTargetInfo.get(targetId)).map(_.javac)
        val scalaStuffOpt          = targetDataElem(_.scalaTargetInfo.get(targetId))
        val scalaInfoOpt           = scalaStuffOpt.map(_.scalaInfo)
        val scalacInfoOpt          = scalaStuffOpt.map(_.scalac)
        val scalaAutoImportsOpt    = scalaStuffOpt.flatMap(_.autoImports)
        val scalaSbtVersionOpt     = scalaStuffOpt.flatMap(_.sbtVersion)
        val inverseDependenciesOpt = targetDataElem(_.inverseDependencies.get(targetId))
        val sourcesOpt = targetDataElem(_.buildTargetSources.get(targetId))
          .map(_.asScala.toVector.map(_.toNIO).map(os.Path(_)).sorted)
        val connectionOpt = targetDataElem(data =>
          if (data.targetToWorkspace.contains(targetId)) data.buildServerOpt else None
        )
        val workspaceOpt = targetDataElem(_.targetToWorkspace.get(targetId))
        printLine(infoOpt.fold("[no info]")(_.toString))
        printLine(javaInfoOpt.fold("[no java info]")(_.toString))
        printLine(scalaInfoOpt.fold("[no scala info]")(_.toString))
        printLine(scalacInfoOpt.fold("[no scalac info]")(_.toString))
        printLine(scalaAutoImportsOpt.fold("[no scalac info]")("Auto imports: " + _.mkString(", ")))
        printLine(scalaSbtVersionOpt.getOrElse("[no sbt version]"))
        printLine(inverseDependenciesOpt.fold("[no inverse dependencies]")(s =>
          "Inverse dependencies:" + System.lineSeparator() + s.map(
            "  " + _ + System.lineSeparator()
          ).mkString
        ).stripSuffix(System.lineSeparator()))
        printLine(sourcesOpt.fold("[no sources]")(s =>
          "Sources:" + System.lineSeparator() + s.map("  " + _ + System.lineSeparator()).mkString
        ).stripSuffix(System.lineSeparator()))
        printLine(connectionOpt.fold("[no connection]")("Connection: " + _))
        printLine(workspaceOpt.fold("[no workspace]")("Workspace: " + _))
      case Inspect.OpenedFiles | Inspect.Opened =>
        val paths = server.editorState.buffers.open
          .map(_.toNIO)
          .map(os.Path(_))
          .filter {
            if (options.readOnly)
              _ => true
            else {
              val readOnlyDir = workingDir / os.SubPath(Directories.readonly.toNIO)
              p => !p.startsWith(readOnlyDir)
            }
          }
          .toVector
          .sorted
        var isFirst = true
        for (path <- paths) {
          if (isFirst)
            isFirst = false
          else
            printLine("")

          printLine {
            val pathStr =
              if (path.startsWith(workingDir)) path.relativeTo(workingDir).toString
              else path.toString
            val maybeFocusedSuffix = {
              val isFocused = server.editorState.focusedDocument.contains(path)
              if (isFocused) " (focused)"
              else ""
            }
            pathStr + maybeFocusedSuffix
          }

          val targetOpt = server.bspData.inverseSources(path)
          targetOpt match {
            case Some(target) =>
              val shortId = BspUtil.targetShortId(server.bspData, target)
              printLine(s"  Build target: $shortId")
              val infoOpt                     = server.compilations.latestCompilations.get(target)
              val lastModified                = os.mtime(path)
              val latestCompilationDetailsOpt = infoOpt.flatMap(_._1)
              val isCompiling                 = infoOpt.flatMap(_._2)
              val diagnostics =
                for {
                  targetId    <- server.bspData.inverseSources0(path).merge
                  buildClient <- server.bspData.buildClientOf(targetId).toSeq
                  diag        <- buildClient.diagnosticsFor(path)
                } yield diag
              latestCompilationDetailsOpt match {
                case Some(latestCompilation) =>
                  val isStale = latestCompilation.startTime.toInstant.toEpochMilli < lastModified
                  val staleMessage =
                    if (isStale) s" (${Console.YELLOW}stale${Console.RESET})"
                    else ""
                  val color =
                    latestCompilation.result.getStatusCode match {
                      case b.StatusCode.OK        => Console.GREEN
                      case b.StatusCode.ERROR     => Console.RED
                      case b.StatusCode.CANCELLED => Console.MAGENTA
                    }
                  printLine(
                    s"  Latest compilation: $color${latestCompilation.result.getStatusCode}${Console.RESET} (${latestCompilation.startTime.atZoneSameInstant(ZoneId.systemDefault()).toLocalDateTime})" + staleMessage
                  )
                case None =>
                  if (isCompiling.isEmpty)
                    printLine(s"  ${Console.RED}No known compilation${Console.RESET}")
              }
              for (compilationDetails <- isCompiling)
                printLine(
                  s"  ${Console.BLUE}Compilation on-going${Console.RESET} since ${compilationDetails.startTime.atZoneSameInstant(ZoneId.systemDefault()).toLocalDateTime}"
                )
              if (options.diagnostics)
                for (diag <- diagnostics)
                  PrintDiagnostic.printFileDiagnostic(printLine(_), Right(path), diag, color = true)
              else {
                val diagnostics0 = diagnostics
                  .groupBy(_.getSeverity)
                  .toVector
                  .sortBy(_._1.getValue)
                if (diagnostics0.nonEmpty)
                  printLine("  " + diagnostics0.map { case (diag, l) =>
                    s"${l.length} $diag"
                  }.mkString(", "))
              }
            case None =>
              printLine(s"  [no build target]")
          }
        }
      // case Inspect.ClassPathIndex =>
      //   var isFirst = true
      //   for (pck <- server.symbolSearchIndex.inDependencies.packages) {
      //     if (isFirst)
      //       isFirst = false
      //     else
      //       printLine("")
      //
      //     for (p <- pck.packages)
      //       printLine(p)
      //   }
      case Inspect.LoadedTargets =>
        val targets = server.bspData.allTargetData.flatMap { data =>
          data.buildTargetInfo.keys.toVector.sortBy(_.getUri)
        }
        for (target <- targets)
          printLine(target.getUri)
      case Inspect.Queues =>
        def maybePrintQueue(name: String, queue: Seq[_]): Unit = {
          if (queue.isEmpty)
            printLine(s"No $name messages queued")
          else {
            printLine(s"  ${name.capitalize}")
            printLine("")
            for (msg <- queue)
              printLine(s"  $msg")
            printLine("")
          }
        }
        val indexerQueue    = indexer.actor.awaitingMessages.toVector
        val bspServersQueue = server.bspServers.actor.awaitingMessages.toVector
        val statusQueue     = server.statusActor.awaitingMessages.toVector
        maybePrintQueue("indexer", indexerQueue)
        maybePrintQueue("BSP servers", bspServersQueue)
        maybePrintQueue("status", statusQueue)

        def inspectPresentationCompilers(
          name: String,
          cache: Map[
            PresentationCompilers.PresentationCompilerKey,
            PresentationCompilers.MtagsPresentationCompiler
          ]
        ): Unit =
          for ((k, v) <- cache.toVector.sortBy(_._1.toString)) {
            val compilerOpt = (v: PresentationCompilers.MtagsPresentationCompiler) match {
              case c: PresentationCompilers.ScalaLazyCompiler => c.compilerOpt
              case c: PresentationCompilers.JavaLazyCompiler  => c.compilerOpt
              case _: PresentationCompilers.LazyCompiler      => ???
            }
            val compilerAccessOpt = compilerOpt.collect {
              case h: HasCompilerAccess => h
            }
            val keyStr = k match {
              case s: PresentationCompilers.PresentationCompilerKey.ScalaBuildTarget =>
                s.id.getUri
              case j: PresentationCompilers.PresentationCompilerKey.JavaBuildTarget =>
                s"${j.id.getUri} (Java)"
            }
            val queueOpt = compilerAccessOpt.map(_.compilerAccess.jobs.jobQueue())
            queueOpt match {
              case Some(queue) =>
                maybePrintQueue(
                  s"$name $keyStr",
                  queue.map(job => s"Job ${job.name} for ${job.uri}")
                )
              case None =>
                printLine(s"Error: cannot get job queue of $name $keyStr")
            }
          }

        inspectPresentationCompilers(
          "presentation compiler",
          server.presentationCompilers.jcache.asScala.toMap
        )
        inspectPresentationCompilers(
          "completion presentation compiler",
          server.presentationCompilers.jCompletionCache.asScala.toMap
        )

        val ongoingRequests = lspServer.ongoingRequests
          .asScala
          .toVector
          .map(_._1)
          .map {
            case (name, params, _) =>
              (name, String.valueOf(params))
          }
          .sorted

        if (ongoingRequests.isEmpty)
          printLine("No ongoing LSP requests")
        else {
          printLine("")
          printLine("  Ongoing LSP requests")
          for ((name, params) <- ongoingRequests)
            printLine(s"$name($params)")
          printLine("")
        }

        val ongoingNotifications = lspServer.ongoingNotifications
          .asScala
          .toVector
          .map(_._1)
          .map {
            case (name, params) =>
              (name, String.valueOf(params))
          }
          .sorted

        if (ongoingNotifications.isEmpty)
          printLine("No ongoing LSP notifications")
        else {
          printLine("")
          printLine("  Ongoing LSP notifications")
          for ((name, params) <- ongoingNotifications)
            printLine(s"$name($params)")
          printLine("")
        }

      case Symbols(maybeShortId) =>
        val targetId = BspUtil.targetFullId(workingDir, maybeShortId)
        printLine(s"Target ${targetId.getUri}")
        printLine("  Found buckets for targets")
        for (t <- server.symbolIndex.dialectBuckets.toVector.map(_._1._2.targetId).sorted.distinct)
          printLine(s"  $t")
        printLine("")
        val buckets = server.symbolIndex.dialectBuckets.collect {
          case ((dialect, module), bucket) if module.targetId == targetId.getUri =>
            (dialect.toString, bucket)
        }
        printLine(s"Found ${buckets.size} symbol bucket(s)")
        for ((dialect, bucket) <- buckets.toVector.sortBy(_._1)) {
          printLine(s"Dialect: $dialect")
          for ((symbol, sources) <- bucket.toplevels.trieMap.toVector.sortBy(_._1)) {
            def sources0 = sources.iterator.map {
              case (path, originOpt) =>
                path.uri + originOpt.fold("")(
                  " (" + _.map(_.toString).left.map(_.toString).merge + ")"
                )
            }
            printLine(symbol)
            for (s <- sources0)
              printLine(s"  $s")
          }
        }

      case other =>
        sys.error(s"Unrecognized inspection target '$other'")
    }
  }
}

object Inspect extends ServerCommand[InspectOptions] {
  def instance(
    server: Server,
    indexer: Indexer,
    client: CommandClient,
    lspServer: plasmon.jsonrpc.JsonrpcServer,
    pool: ServerCommandThreadPools
  ): ServerCommandInstance[InspectOptions] =
    Inspect(server, indexer, client, lspServer)

  object BuildTargetData {
    def prefix = "build-target-data:"
    def unapply(input: String): Option[String] =
      if (input.startsWith(prefix)) Some(input.stripPrefix(prefix))
      else None
  }
  val CompilerCache = "compiler-cache"
  val Opened        = "opened"
  val OpenedFiles   = "opened-files"
  // val ClassPathIndex = "class-path-index"
  val LoadedTargets = "loaded-targets"
  val Queues        = "queues"
  object Symbols {
    def prefix = "symbols:"
    def unapply(input: String): Option[String] =
      if (input.startsWith(prefix)) Some(input.stripPrefix(prefix))
      else None
  }
}
