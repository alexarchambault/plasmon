package plasmon.servercommand

import caseapp.core.RemainingArgs
import plasmon.command.ServerCommandThreadPools
import plasmon.protocol.CommandClient
import plasmon.bsp.{BuildServerInfo, BuildTool}
import plasmon.index.Indexer
import plasmon.Server
import scala.concurrent.Await
import scala.concurrent.duration.Duration

final case class BuildToolAdd(
  server: Server,
  client: CommandClient,
  pool: ServerCommandThreadPools
) extends ServerCommandInstance[BuildToolAddOptions](client) {
  override def names = BuildToolAdd.names
  def run(options: BuildToolAddOptions, args: RemainingArgs): Unit = {

    val buildTool = BuildToolAdd.buildTool(server.workingDir, options, args.all)

    val f = server.bspServers.add(
      buildTool,
      printLine(_, toStderr = true),
      pool.bspEces,
      () => pool.bloopThreads,
      server.tools
    )
    val res = Await.result(f, Duration.Inf)
    res match {
      case Left(err) =>
        printLine(err, toStderr = true)
        exit(1)
      case Right(()) =>
        server.bspServers.persist()
    }
  }
}

object BuildToolAdd extends ServerCommand[BuildToolAddOptions] {
  def instance(
    server: Server,
    indexer: Indexer,
    client: CommandClient,
    lspServer: plasmon.jsonrpc.JsonrpcServer,
    pool: ServerCommandThreadPools
  ): ServerCommandInstance[BuildToolAddOptions] =
    BuildToolAdd(server, client, pool)
  override def names = List(
    List("build", "tool", "add"),
    List("build-tool", "add"),
    List("build-tool-add")
  )

  sealed abstract class BuildToolType extends Product with Serializable
  case object Bsp                     extends BuildToolType
  case object Mill                    extends BuildToolType
  case object MillViaBloop            extends BuildToolType
  case object Sbt                     extends BuildToolType
  case object SbtViaBloop             extends BuildToolType
  case object Bloop                   extends BuildToolType
  case object ScalaCli                extends BuildToolType

  def buildToolType(options: BuildToolAddOptions): BuildToolType =
    if (options.shared.mill.getOrElse(false)) Mill
    else if (options.shared.millViaBloop.getOrElse(false)) MillViaBloop
    else if (options.sbt.getOrElse(false)) Sbt
    else if (options.shared.bloop.getOrElse(false)) Bloop
    else if (options.shared.scalaCli.getOrElse(false)) ScalaCli
    else Bsp

  def buildTool(
    workingDir: os.Path,
    options: BuildToolAddOptions,
    args: Seq[String]
  ): BuildTool = {

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

    buildToolType(options) match {
      case Bsp =>
        val bspFile = options.shared.bspFile
          .filter(_.trim.nonEmpty)
          .map(os.Path(_, workingDir))
          .getOrElse(BspUtil.bspFile(path))
        BuildTool.Bsp(
          BuildServerInfo.Bsp(
            path,
            bspFile,
            Some(options.shared.onlyTarget)
              .filter(_.nonEmpty)
              .map(_.map(BspUtil.targetFullId(path, _).getUri).toSet)
          )
        )
      case Bloop =>
        BuildTool.Bloop(path)
      case MillViaBloop =>
        BuildTool.MillViaBloop(path)
      case Mill =>
        BuildTool.Mill(path)
      // Some(options.shared.onlyTarget)
      //   .filter(_.nonEmpty)
      //   .map(_.map(BspUtil.targetFullId(path, _).getUri).toSet)
      case Sbt =>
        BuildTool.Sbt(path)
      case SbtViaBloop =>
        BuildTool.SbtViaBloop(path)
      case ScalaCli =>
        val workspace = if (os.isFile(paths.head)) paths.head / os.up else paths.head
        BuildTool.ScalaCli(workspace, paths)
    }
  }
}
