package plasmon.servercommand

import caseapp.core.RemainingArgs
import ch.epfl.scala.{bsp4j => b}
import plasmon.index.Indexer
import plasmon.protocol.CommandClient
import plasmon.protocol.CommandClient.ops.*
import plasmon.Server

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

import plasmon.PlasmonEnrichments._
import scala.jdk.CollectionConverters._

final case class BspClean(
  server: Server,
  client: CommandClient,
  dummyEc: ExecutionContext
) extends ServerCommandInstance[BspCleanOptions](client) {
  override def names = BspClean.names
  def run(options: BspCleanOptions, args: RemainingArgs): Unit = {

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

    val futures = targetsByConnection.map {
      case (conn, targets) =>
        val compileParams                 = new b.CleanCacheParams(targets.toList.asJava)
        implicit val ec: ExecutionContext = dummyEc
        conn.buildTargetCleanCache(compileParams).asScala.map(conn -> _)
    }
    val future = {
      implicit val ec: ExecutionContext = dummyEc
      Future.sequence(futures)
    }
    val results = Await.result(future, Duration.Inf)

    var hasErrors = false
    for ((conn, res) <- results) {
      for (msg <- Option(res.getMessage))
        printLine(msg, toStderr = true)
      if (!res.getCleaned) {
        printLine(s"Error cleaning on $conn", toStderr = true)
        hasErrors = true
      }
    }

    if (hasErrors)
      exit(1)
  }
}

object BspClean extends ServerCommand[BspCleanOptions] {
  def instance(
    server: Server,
    indexer: Indexer,
    client: CommandClient,
    lspServer: plasmon.jsonrpc.JsonrpcServer,
    pool: plasmon.command.ServerCommandThreadPools
  ): ServerCommandInstance[BspCleanOptions] =
    BspClean(server, client, pool.dummyEc)
  override def names = List(
    List("bsp", "clean"),
    List("bsp-clean")
  )
}
