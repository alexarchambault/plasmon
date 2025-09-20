package plasmon.servercommand

import caseapp.core.RemainingArgs
import plasmon.command.ServerCommandThreadPools
import plasmon.index.Indexer
import plasmon.protocol.CommandClient
import plasmon.bsp.{BuildServerInfo, BuildTool}
import plasmon.Server
import scala.concurrent.Await
import scala.concurrent.duration.Duration

final case class BspAdd(
  server: Server,
  client: CommandClient,
  pool: ServerCommandThreadPools
) extends ServerCommandInstance[BspAddOptions](client) {
  override def names = BspAdd.names
  def run(options: BspAddOptions, args: RemainingArgs): Unit = {

    val info0 = BspAdd.info(server.workingDir, options.shared, args.all, server.tools)

    val resF = server.bspServers.add(
      BuildTool.Bsp(info0),
      printLine(_, toStderr = true),
      pool.bspEces,
      () => pool.bloopThreads,
      server.tools
    )
    Await.result(resF, Duration.Inf) match {
      case Left(err) =>
        printLine(err, toStderr = true)
        exit(1)
      case Right(()) =>
        server.bspServers.persist()
    }
  }
}

object BspAdd extends ServerCommand[BspAddOptions] {
  def instance(
    server: Server,
    indexer: Indexer,
    client: CommandClient,
    lspServer: plasmon.jsonrpc.JsonrpcServer,
    pool: ServerCommandThreadPools
  ): ServerCommandInstance[BspAddOptions] =
    BspAdd(server, client, pool)
  override def names = List(
    List("bsp", "add"),
    List("bsp-add")
  )

  sealed abstract class BspImportType extends Product with Serializable
  case object Bsp                     extends BspImportType
  case object Mill                    extends BspImportType
  case object Bloop                   extends BspImportType
  case object ScalaCli                extends BspImportType

  def bspImportType(options: SharedBspAddRemoveOptions): BspImportType =
    if (options.mill.getOrElse(false)) Mill
    else if (options.bloop.getOrElse(false)) Bloop
    else if (options.scalaCli.getOrElse(false)) ScalaCli
    else Bsp

  def info(
    workingDir: os.Path,
    options: SharedBspAddRemoveOptions,
    args: Seq[String],
    tools: BuildTool.Tools
  ): BuildServerInfo = {

    lazy val path = args match {
      case Seq()        => workingDir
      case Seq(strPath) => os.Path(strPath, workingDir)
      case _ =>
        sys.error(s"Too many paths passed (expected at most one)")
    }

    lazy val paths = args match {
      case Seq() => Seq(workingDir)
      case other => other.map(os.Path(_, workingDir))
    }

    bspImportType(options) match {
      case Bsp =>
        val bspFile = options.bspFile
          .filter(_.trim.nonEmpty)
          .map(os.Path(_, workingDir))
          .getOrElse(BspUtil.bspFile(path))
        BuildServerInfo.Bsp(
          path,
          bspFile,
          Some(options.onlyTarget)
            .filter(_.nonEmpty)
            .map(_.map(BspUtil.targetFullId(path, _).getUri).toSet)
        )
      case Bloop =>
        BuildServerInfo.Bloop(path)
      case Mill =>
        BuildServerInfo.Mill(
          path,
          Some(options.onlyTarget)
            .filter(_.nonEmpty)
            .map(_.map(BspUtil.targetFullId(path, _).getUri).toSet)
        )
      case ScalaCli =>
        val workspace = if (os.isFile(paths.head)) paths.head / os.up else paths.head
        BuildServerInfo.ScalaCli(
          workspace,
          paths,
          tools.tools.getOrElse("scala-cli", sys.error("scala-cli not found in tools"))
        )
    }
  }
}
