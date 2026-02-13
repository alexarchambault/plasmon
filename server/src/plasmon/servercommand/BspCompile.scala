package plasmon.servercommand

import caseapp.core.RemainingArgs
import ch.epfl.scala.{bsp4j => b}
import plasmon.index.Indexer
import plasmon.protocol.CommandClient
import plasmon.protocol.CommandClient.ops.*
import plasmon.util.PrintDiagnostic
import plasmon.bsp.PlasmonBuildClient
import plasmon.Server

import java.net.URI
import java.util.UUID

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

import plasmon.PlasmonEnrichments._
import scala.jdk.CollectionConverters._
import org.eclipse.lsp4j.jsonrpc.ResponseErrorException
import java.util.concurrent.CompletionException

final case class BspCompile(
  server: Server,
  client: CommandClient,
  dummyEc: ExecutionContext
) extends ServerCommandInstance[BspCompileOptions](client) {
  override def names = BspCompile.names
  def run(options: BspCompileOptions, args: RemainingArgs): Unit = {

    val path = options.sharedBsp.workspace
      .filter(_.trim.nonEmpty)
      .map(os.Path(_, server.workingDir))
      .getOrElse(server.workingDir)

    val targetsByConnection = BspUtil.targetsByConnection(
      server.bspData.allTargetData,
      client.printLine(_, toStderr = true),
      path,
      options.sharedBsp
    )

    val compilationClient: PlasmonBuildClient.CompilationClient =
      new PlasmonBuildClient.CompilationClient {
        def onBuildPublishDiagnostics(params: b.PublishDiagnosticsParams): Unit = {
          // FIXME Source mapping
          val path = params.getTextDocument.getUri.osPathFromUri
          for (diag <- params.getDiagnostics.asScala)
            PrintDiagnostic.printFileDiagnostic(printLine(_), Right(path), diag, color = true)
        }
      }

    val buildClients = {
      val buildServers = targetsByConnection.map(_._1).toSet
      server.bspServers.list
        .flatMap(_._2)
        .filter(conn => buildServers.contains(conn.conn))
        .map(_.client)
    }

    val originId = s"plasmon-${UUID.randomUUID()}"
    val compileResults =
      try {
        buildClients.foreach(_.addClient(originId, compilationClient))

        val futures = targetsByConnection.map {
          case (conn, targets) =>
            val compileParams = new b.CompileParams(targets.toList.asJava)
            compileParams.setOriginId(originId)
            val f = conn.buildTargetCompile(compileParams).asScala
            if (options.dumbBuildToolHacks)
              // compilation error improperly returned as LSP error
              f.recover {
                case e: CompletionException
                    if e.getCause.isInstanceOf[ResponseErrorException] &&
                    e.getCause
                      .asInstanceOf[ResponseErrorException]
                      .getMessage
                      .contains("Compilation failed") =>
                  new b.CompileResult(b.StatusCode.ERROR)
              }(using dummyEc)
            else
              f
        }
        val future = {
          implicit val ec: ExecutionContext = dummyEc
          Future.sequence(futures)
        }
        Await.result(future, Duration.Inf)
      }
      finally
        buildClients.foreach(_.removeClient(originId))
    val statuses = compileResults.map(_.getStatusCode)
    if (statuses.contains(b.StatusCode.ERROR))
      client.printLine("Compilation error")
    else if (statuses.contains(b.StatusCode.CANCELLED))
      client.printLine("Compilation cancelled")
    else
      client.printLine("Done")
  }
}

object BspCompile extends ServerCommand[BspCompileOptions] {
  def instance(
    server: Server,
    indexer: Indexer,
    client: CommandClient,
    lspServer: plasmon.jsonrpc.JsonrpcServer,
    pool: plasmon.command.ServerCommandThreadPools
  ): ServerCommandInstance[BspCompileOptions] =
    BspCompile(server, client, pool.dummyEc)
  override def names = List(
    List("bsp", "compile"),
    List("bsp-compile")
  )
}
