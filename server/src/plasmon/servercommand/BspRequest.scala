package plasmon.servercommand

import caseapp.core.RemainingArgs
import ch.epfl.scala.{bsp4j => b}
import com.google.gson.Gson
import org.eclipse.lsp4j.jsonrpc.MessageIssueException
import plasmon.protocol.CommandClient
import plasmon.protocol.CommandClient.ops.*
import plasmon.index.Indexer
import plasmon.Server

import java.util.concurrent.{CompletableFuture, ExecutionException, ExecutorService}

import scala.jdk.CollectionConverters.*
import scala.build.bsp.WrappedSourcesParams

final case class BspRequest(
  server: Server,
  client: CommandClient,
  bspPool: ExecutorService
) extends ServerCommandInstance[BspRequestOptions](client) {
  override def names = BspRequest.names
  def run(options: BspRequestOptions, args: RemainingArgs): Unit = {

    val buildServer = options.buildServer match {
      case None =>
        server.bspServers.list.flatMap(_._2) match {
          case Seq(s) => s
          case Seq()  => sys.error("No build server found")
          case other  => sys.error(s"Found too many build servers (${other.length}), expected 1")
        }
      case Some(buildServerName) =>
        server.bspServers.list.flatMap(_._2).find(_.name == buildServerName) match {
          case Some(buildServer) => buildServer
          case None =>
            sys.error(s"Build server '$buildServerName' not found (current build servers: ${server.bspServers.list.flatMap(
                _._2
              ).map(_.name).mkString(", ")})")
        }
    }

    val (requestName, requestArgs) = args.all match {
      case Seq() => sys.error("No request name specified")
      case Seq(name0, args0 @ _*) =>
        (name0, args0)
    }

    def targetIdFromArg(arg: String): b.BuildTargetIdentifier = {
      val uri =
        if (arg.contains("://")) arg
        else os.Path(arg, os.pwd).toNIO.toUri.toASCIIString.stripSuffix("/")
      new b.BuildTargetIdentifier(uri)
    }

    def textDocumentIdFromArg(arg: String): b.TextDocumentIdentifier = {
      val uri =
        if (arg.contains("://")) arg
        else os.Path(arg, os.pwd).toNIO.toUri.toASCIIString
      new b.TextDocumentIdentifier(uri)
    }

    def textDocumentId =
      requestArgs.map(textDocumentIdFromArg).toList match {
        case Nil      => sys.error("No argument passed (expected one)")
        case h :: Nil => h
        case _        => sys.error("Too many arguments passed (expected one)")
      }

    def targetId =
      requestArgs.map(targetIdFromArg).toList match {
        case Nil      => sys.error("No argument passed (expected one)")
        case h :: Nil => h
        case _        => sys.error("Too many arguments passed (expected one)")
      }

    def targetIds =
      requestArgs.map(targetIdFromArg).toList.asJava

    val res: CompletableFuture[?] = requestName match {
      case "workspace/buildTargets" | "buildTargets" =>
        buildServer.remoteEndpoint.request("workspace/buildTargets", null)
      case "workspace/reload" | "reload" =>
        buildServer.remoteEndpoint.request("workspace/reload", null)
      case "buildTarget/sources" | "sources" =>
        val params = new b.SourcesParams(targetIds)
        buildServer.conn.buildTargetSources(params)
      case "buildTarget/inverseSources" | "inverseSources" =>
        val params = new b.InverseSourcesParams(textDocumentId)
        buildServer.conn.buildTargetInverseSources(params)
      case "buildTarget/dependencySources" | "dependencySources" =>
        val params = new b.DependencySourcesParams(targetIds)
        buildServer.conn.buildTargetDependencySources(params)
      case "buildTarget/dependencyModules" | "dependencyModules" =>
        val params = new b.DependencyModulesParams(targetIds)
        buildServer.conn.buildTargetDependencyModules(params)
      case "buildTarget/resources" | "resources" =>
        val params = new b.ResourcesParams(targetIds)
        buildServer.conn.buildTargetResources(params)
      case "buildTarget/outputPaths" | "outputPaths" =>
        val params = new b.OutputPathsParams(targetIds)
        buildServer.conn.buildTargetOutputPaths(params)
      case "buildTarget/compile" | "compile" =>
        val params = new b.CompileParams(targetIds)
        buildServer.conn.buildTargetCompile(params)
      case "buildTarget/run" | "run" =>
        val params = new b.RunParams(targetId)
        buildServer.conn.buildTargetRun(params)
      case "buildTarget/test" | "test" =>
        val params = new b.TestParams(targetIds)
        buildServer.conn.buildTargetTest(params)
      case "buildTarget/cleanCache" | "cleanCache" =>
        val params = new b.CleanCacheParams(targetIds)
        buildServer.conn.buildTargetCleanCache(params)
      case "buildTarget/wrappedSources" | "wrappedSources" =>
        val params = new WrappedSourcesParams(targetIds)
        buildServer.conn.buildTargetWrappedSources(params)

      case "buildTarget/scalacOptions" | "scalacOptions" =>
        val params = new b.ScalacOptionsParams(targetIds)
        buildServer.conn.buildTargetScalacOptions(params)

      case _ =>
        sys.error(s"Unrecognized request name: $requestName")
    }
    val message =
      try res.get()
      catch {
        case e: ExecutionException =>
          e.getCause match {
            case e0: MessageIssueException =>
              printLine(e0.getRpcMessage.getJsonrpc)
              printLine(s"Error decoding response: ${e0.getMessage}", toStderr = true)
              exit(1)
            case _ =>
          }
          throw e
      }
    val jsonStr = new Gson().toJson(message)

    client.printLine(jsonStr)
  }
}

object BspRequest extends ServerCommand[BspRequestOptions] {
  override def names = List(
    List("bsp", "request"),
    List("bsp-request")
  )
  def instance(
    server: Server,
    indexer: Indexer,
    client: CommandClient,
    lspServer: plasmon.jsonrpc.JsonrpcServer,
    pool: plasmon.command.ServerCommandThreadPools
  ): ServerCommandInstance[BspRequestOptions] =
    BspRequest(server, client, pool.bspEces)
}
