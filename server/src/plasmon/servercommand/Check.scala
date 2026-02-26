package plasmon.servercommand

import caseapp.core.RemainingArgs
import ch.epfl.scala.bsp4j as b
import org.eclipse.lsp4j as l
import plasmon.Server
import plasmon.PlasmonEnrichments.*
import plasmon.bsp.PlasmonBuildClient
import plasmon.command.ServerCommandThreadPools
import plasmon.index.Indexer
import plasmon.protocol.CommandClient
import plasmon.semdb.TextDocumentLookup
import plasmon.util.PrintDiagnostic

import java.io.{ByteArrayOutputStream, PrintStream}
import java.net.URI
import java.nio.charset.StandardCharsets
import java.util.UUID

import scala.concurrent.{Await, Promise}
import scala.concurrent.duration.Duration
import scala.jdk.CollectionConverters.*
import scala.meta.cli.Reporter
import scala.meta.internal.metap.DocumentPrinter
import scala.meta.internal.mtags.SourcePath
import scala.meta.internal.semanticdb.{Language, TextDocument}
import scala.meta.metap.Settings

final case class Check(
  server: Server,
  client: CommandClient
) extends ServerCommandInstance[CheckOptions](client) {
  def run(options: CheckOptions, args: RemainingArgs): Unit = {
    val constraints = options.require
      .map(_.trim)
      .filter(_.nonEmpty)
      .flatMap(BspUtil.TagConstraint.parse)

    val allBuildServers = server.bspServers.list.flatMap(_._2).map(_.conn)
    lazy val allTargets = allBuildServers
      .map { buildServer =>
        val allTargets0 = buildServer
          .workspaceBuildTargets
          .get()
          .getTargets
          .asScala
          .toList
        buildServer -> allTargets0
      }
      .toMap
    val workingDir = server.workingDir
    for (strPath <- args.all) {
      val path      = os.Path(strPath, workingDir)
      val textDocId = new b.TextDocumentIdentifier(path.toNIO.toUri.toASCIIString)
      val allPathTargets = allBuildServers
        .iterator
        .flatMap { buildServer =>
          buildServer
            .buildTargetInverseSources(
              new b.InverseSourcesParams(textDocId)
            )
            .get()
            .getTargets
            .asScala
            .iterator
            .map(_ -> buildServer)
        }
        .toMap
      val (targetId, buildServer) = options.target match {
        case Some(target) =>
          val targetId0 = BspUtil.targetFullId(workingDir, target)
          val buildServer0 = allTargets.find(_._2.contains(targetId0)).map(_._1).getOrElse {
            printLine(s"Target $target not found for $strPath ($allTargets)")
            exit(1)
          }
          (targetId0, buildServer0)
        case None =>
          if (allPathTargets.isEmpty) {
            printLine(s"Error: cannot find build target of $strPath", toStderr = true)
            exit(1)
          }
          else if (allPathTargets.size > 1) {
            val buildServerCount = allPathTargets.values.toVector.distinct.size
            assert(buildServerCount > 0)
            if (buildServerCount > 1) {
              printLine(s"Error: too many build servers for $strPath ($allPathTargets)")
              exit(1)
            }
            val (_, buildServer) = allPathTargets.head
            val targetDataMap = allTargets(buildServer).map(target => target.getId -> target).toMap
            val filtered = allPathTargets
              .keysIterator
              .flatMap { targetId =>
                val tags = BspUtil.tags(targetDataMap(targetId))
                scribe.info(s"Target ${targetId.getUri} has tags $tags")
                if (constraints.forall(_.validate(tags)))
                  Iterator(targetId)
                else {
                  scribe.info(s"Not keeping target ${targetId.getUri} (tags don't fit constraints)")
                  Iterator.empty
                }
              }
              .toVector
            val targetId = filtered match {
              case Seq() =>
                printLine(
                  s"Error: cannot find build target of $strPath after filtering (before filtering: $allPathTargets)",
                  toStderr = true
                )
                exit(1)
              case Seq(targetId0) =>
                targetId0
              case _ =>
                printLine(s"Too many targets for $strPath ($allPathTargets)", toStderr = true)
                exit(1)
            }
            (targetId, buildServer)
          }
          else
            allPathTargets.head
      }

      val dependenciesMap = allTargets(buildServer)
        .map(target => target.getId -> target.getDependencies.asScala.toSet)
        .toMap
      val finalTargets = BspUtil.addTransitiveDependencies(Set(targetId), dependenciesMap)
        .toVector
        .sortBy(BspUtil.targetShortId(server.bspData, _))
      printLine(s"Retained ${finalTargets.length} build targets with dependencies", toStderr = true)

      val missing =
        finalTargets.filter(id =>
          !server.bspData.allTargetData.exists(_.buildTargetInfo.contains(id))
        )
      if (missing.nonEmpty)
        sys.error(s"Non-loaded targets: ${missing.toVector.map(_.getUri).mkString(", ")}")

      val originId = s"plasmon-${UUID.randomUUID()}"
      val compilationClient: PlasmonBuildClient.CompilationClient =
        new PlasmonBuildClient.CompilationClient {
          def onBuildPublishDiagnostics(params: b.PublishDiagnosticsParams): Unit = {
            // FIXME Source mapping
            val path = params.getTextDocument.getUri.osPathFromUri
            for (diag <- params.getDiagnostics.asScala)
              PrintDiagnostic.printFileDiagnostic(printLine(_), Right(path), diag, color = true)
          }
        }

      val buildClient = server.bspServers
        .list
        .iterator
        .flatMap(_._2.iterator)
        .find(_.conn == buildServer)
        .map(_.client)
        .getOrElse {
          sys.error("No build client found (should not happen)")
        }

      printLine(s"Compiling ${BspUtil.targetShortId(server.bspData, targetId)}")
      val compileResults =
        try {
          buildClient.addClient(originId, compilationClient)

          val compileParams = new b.CompileParams(List(targetId).asJava)
          compileParams.setOriginId(originId)
          buildServer.buildTargetCompile(compileParams).get()
        }
        finally
          buildClient.removeClient(originId)

      val status = compileResults.getStatusCode
      if (status == b.StatusCode.ERROR) {
        printLine("Compilation error")
        exit(1)
      }
      else if (status == b.StatusCode.CANCELLED) {
        printLine("Compilation cancelled")
        exit(1)
      }
      else
        printLine("Compilation done")

      printLine(s"Checking for $strPath semantic DB")
      val semdbRes =
        server.fileSystemSemanticdbs.textDocument0(path, targetId)
      semdbRes.flatMap(_.documentIncludingStaleE) match {
        case Left(err) =>
          printLine(s"No semantic DB found for $strPath: $err")
          exit(1)
        case Right(semdb) =>
          def path = semdbRes match {
            case Right(s: TextDocumentLookup.Success) => s.path
            case _                                    => None
          }
          printLine {
            if (options.verbose)
              s"Found semantic DB for $strPath at $path"
            else
              s"Found semantic DB for $strPath"
          }

          if (options.metap)
            printLine(Check.metap(semdb))

          semdb.language match {
            case Language.UNKNOWN_LANGUAGE =>
              printLine("Warning: semantic DB has unknown language")
              exit(1)
            case other =>
              printLine(s"Semantic DB has language $other")
          }
      }

      printLine(s"Compiling $strPath with interactive compiler")
      val dummyToken = {
        import scala.concurrent.ExecutionContext.Implicits.global
        import plasmon.ide.FutureCancelToken
        FutureCancelToken(Promise[Boolean]().future)
      }
      val futureResOpt = server.presentationCompilers.compile(
        new l.TextDocumentIdentifier(textDocId.getUri),
        targetId,
        dummyToken
      )
      futureResOpt match {
        case None =>
          sys.error(s"No compiler found for $path (${targetId.getUri})")
        case Some(futureRes) =>
          val res = Await.result(futureRes, Duration.Inf)
          if (options.fullTree)
            printLine(res.fullTree)
          for (diag <- res.diagnostics.asScala)
            printLine(diag)
      }

      printLine(s"Checking for $strPath interactive semantic DB")
      val interactiveSemdb = server.computeInteractiveSemanticdb(
        path,
        server.editorState.buffers.get(path).getOrElse(os.read(path)),
        targetId
      )

      if (options.interactiveMetap)
        printLine(Check.metap(interactiveSemdb))

      interactiveSemdb.language match {
        case Language.UNKNOWN_LANGUAGE =>
          printLine("Warning: interactive semantic DB has unknown language")
          exit(1)
        case other =>
          printLine(s"Interactive semantic DB has language $other")
      }
    }
  }
}

object Check extends ServerCommand[CheckOptions] {
  def instance(
    server: Server,
    indexer: Indexer,
    client: CommandClient,
    lspServer: plasmon.jsonrpc.JsonrpcServer,
    pool: ServerCommandThreadPools
  ): ServerCommandInstance[CheckOptions] =
    Check(server, client)

  private def metap(doc: TextDocument): String = {
    val baos = new ByteArrayOutputStream
    val ps   = new PrintStream(baos, true, StandardCharsets.UTF_8)
    new DocumentPrinter(Settings(), Reporter().withOut(ps).withErr(ps), doc)
      .print()
    ps.flush()
    new String(baos.toByteArray, StandardCharsets.UTF_8)
  }
}
