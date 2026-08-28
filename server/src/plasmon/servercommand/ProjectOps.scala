package plasmon.servercommand

import ch.epfl.scala.bsp4j as b
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import coursier.version.Version
import org.eclipse.lsp4j.jsonrpc.ResponseErrorException
import org.eclipse.lsp4j.jsonrpc.messages.ResponseErrorCode
import plasmon.Server
import plasmon.PlasmonEnrichments.*
import plasmon.command.ServerCommandThreadPools
import plasmon.index.Indexer
import plasmon.internal.Constants

import java.util.concurrent.ExecutionException

import scala.build.bsp.{WrappedSourcesParams, WrappedSourcesResult}
import scala.concurrent.Future
import scala.jdk.CollectionConverters.*
import scala.util.{Failure, Success}

/** The project-level operations behind both ways of driving the server.
  *
  * Everything here is reached twice: once from the LSP commands in
  * [[plasmon.handlers.PlasmonCommands]] (what an editor extension calls), and once from the
  * corresponding CLI commands in this package (what `plasmon …` in a terminal calls). Keeping the
  * logic here rather than in either entry point is what makes the two equivalent - the integration
  * tests run the same scenarios both ways and compare against the same fixtures.
  */
object ProjectOps {

  private final case class OppositeOrdering[T <: Ordered[T]](value: T)
      extends Ordered[OppositeOrdering[T]] {
    def compare(that: OppositeOrdering[T]): Int =
      -value.compare(that.value)
  }

  /** A build target that a file belongs to, and could be loaded from. */
  final case class ModuleInfo(
    workspace: String,
    server: String,
    uri: String,
    label: String,
    detail: String,
    description: String,
    alreadyLoaded: Boolean
  )

  object ModuleInfo {
    implicit lazy val codec: JsonValueCodec[ModuleInfo]         = JsonCodecMaker.make
    implicit lazy val seqCodec: JsonValueCodec[Seq[ModuleInfo]] = JsonCodecMaker.make
  }

  def loadBuildTool(
    server: Server,
    pools: ServerCommandThreadPools,
    discoverId: String,
    toolId: String,
    currentFileOpt: Option[os.Path]
  ): Future[Either[String, Unit]] = {
    implicit val ec = server.pools.requestsEces
    val f = Future {
      BspUtil.BuildToolDiscover.map.get(discoverId) match {
        case Some(discover) =>
          val tools = discover.check(
            server.workspace(),
            currentFileOpt,
            server.bspServers.list.map(_._1).toSet
          )
          tools.find(_.buildTool.id == toolId) match {
            case Some(tool) =>
              val f = server.bspServers.tryAdd(
                tool.buildTool,
                tool.buildTool.launcher(server.tools) +: tool.buildTool.extraLaunchers,
                line => scribe.info("BSP: " + line),
                pools.bspEces,
                () => pools.bloopThreads
              )
              f.flatMap {
                case Left(err) =>
                  Future.successful(Left(err))
                case Right(()) =>
                  Future {
                    server.bspServers.persist()
                    Right(())
                  }
              }
            case None =>
              val found = tools.map(tool => (tool.buildTool.id, tool.discoverId))
              scribe.error(s"Build tool $toolId / $discoverId not found, available tools: $found")
              scribe.error(s"currentFileOpt=$currentFileOpt")
              Future.successful(
                Left(s"Build tool $toolId / $discoverId not found (internal error)")
              )
          }
        case None =>
          Future.successful(Left(s"Build tool $discoverId not found"))
      }
    }
    f.flatten
  }

  /** The build tools that could be loaded for a workspace, from the point of view of `fileOpt`. */
  def discoverBuildTools(
    server: Server,
    fileOpt: Option[os.Path]
  ): Seq[BspUtil.DiscoveredBuildTool] = {
    val alreadyAdded = server.bspServers.list.map(_._1).toSet
    BspUtil
      .discoverBuildTools(server.workspace(), fileOpt, alreadyAdded)
      .filter(tool => !alreadyAdded.contains(tool.buildTool))
  }

  /** The not-yet-loaded modules a file belongs to, best candidate first. */
  def listModules(
    file: os.Path,
    server: Server
  ): Seq[ModuleInfo] = {
    val loadedTargetIds = server.bspData.allTargetData
      .flatMap { targetData =>
        scribe.info(s"targetData.sourceBuildTargets($file)=" + pprint.apply(
          targetData.sourceBuildTargets(file)
        ))
        targetData.sourceBuildTargets(file)
          .toSeq
          .flatMap(_.toVector.sortBy(_.getUri))
      }
      .toSet
    server.bspServers.list.flatMap(_._2).flatMap { server0 =>
      scribe.info(s"server0.name=" + pprint.apply(server0.name))
      val workspaceBuildTargetsRes = server0.conn.workspaceBuildTargets().get()
      val targetMap = workspaceBuildTargetsRes.getTargets.asScala.map(t => t.getId -> t).toMap
      val targets =
        if (server0.name == "sbt") {
          // No inverseSources support? sbt sucks, as usual
          val sourcesRes = server0.conn
            .buildTargetSources(
              new b.SourcesParams(workspaceBuildTargetsRes.getTargets.asScala.map(_.getId).asJava)
            )
            .get()
          scribe.info("sourcesRes=" + pprint.apply(sourcesRes))
          sourcesRes
            .getItems
            .asScala
            .flatMap { item =>
              val matches = item.getSources.asScala.exists { item0 =>
                val path = item0.getUri.osPathFromUri
                file.startsWith(path)
              }
              if (matches) Seq(item.getTarget)
              else Nil
            }
        }
        else {
          val res = server0.conn
            .buildTargetInverseSources(
              new b.InverseSourcesParams(
                new b.TextDocumentIdentifier(file.toNIO.toUri.toASCIIString)
              )
            )
            .get()
          if (
            server0.name == "mill-bsp" &&
            res.getTargets.asScala.isEmpty && file.startsWith(server0.info.workspace) &&
            (
              file.last.endsWith(".sc") ||
              file.last.endsWith(".mill") ||
              file.last.endsWith(".mill.scala")
            )
          ) {
            // working around a buildTargetInverseSources bug in Mill with its mill-build target
            // fixed in Mill 1.0.4 by com-lihaoyi/mill#5698
            val targetId = new b.BuildTargetIdentifier(
              server0.info.workspace.toNIO.toUri.toASCIIString + "mill-build"
            )
            val sourcesResp =
              server0.conn.buildTargetSources(new b.SourcesParams(List(targetId).asJava)).get()
            val isMillBuildSource = sourcesResp.getItems.asScala.iterator
              .filter(_.getTarget == targetId)
              .flatMap(_.getSources.asScala.iterator)
              .map(_.getUri.osPathFromUri)
              .exists(_ == file)
            if (isMillBuildSource) Seq(targetId) else Nil
          }
          else
            res.getTargets.asScala.toVector
        }
      val targets0 =
        if (targets.isEmpty && (file.last.endsWith(".sc") || file.last.endsWith(".mill"))) {

          val targetList = server.bspData.allWritableData.iterator
            .filter(_.buildServerOpt.contains(server0.conn))
            .flatMap(_.targetToWorkspace.keys)
            .toList
            .asJava

          val wrappedSourcesRes =
            try
              server0.conn
                .buildTargetWrappedSources(new WrappedSourcesParams(targetList))
                .get()
            catch {
              case e: ExecutionException =>
                e.getCause match {
                  case ex: ResponseErrorException
                      if ex.getResponseError.getCode == ResponseErrorCode.MethodNotFound.getValue =>
                    scribe.warn(
                      s"wrappedSources method not supported by ${server0.info}, ignoring it"
                    )
                    new WrappedSourcesResult(Nil.asJava)
                  case _ =>
                    throw e
                }
            }

          scribe.info(s"Looking for ${file.toNIO.toUri.toASCIIString} in $wrappedSourcesRes")

          wrappedSourcesRes.getItems.asScala.iterator
            .filter(_.getSources.asScala.exists(_.getUri == file.toNIO.toUri.toASCIIString))
            .map(_.getTarget)
            .toVector
        }
        else targets
      val retainedTargets = {
        val l = targets0
          .collect {
            case id if !loadedTargetIds.contains(id) =>
              id
          }
          .toVector
        val l0 = l.sortBy { id =>
          val scalaTargetOpt = targetMap.get(id)
            .orElse {
              server.bspData.targetData(server0.info).flatMap(_.buildTargetInfo.get(id))
            }
            .flatMap(_.asScalaBuildTarget)
          val scalaVer =
            scalaTargetOpt.map(_.getScalaVersion).map(Version(_)).getOrElse(Version("0"))
          val platformIdx = scalaTargetOpt.map(_.getPlatform.getValue).getOrElse(0)
          (OppositeOrdering(scalaVer), platformIdx, id.getUri)
        }
        l0.zipWithIndex.map {
          case (id, idx) =>
            val recommended = idx == 0 && {
              val scalaTargetOpt = targetMap.get(id)
                .orElse {
                  server.bspData.targetData(server0.info).flatMap(_.buildTargetInfo.get(id))
                }
                .flatMap(_.asScalaBuildTarget)
              val scalaVerOpt = scalaTargetOpt.map(_.getScalaVersion)
              scalaVerOpt.forall { sv =>
                sv.startsWith("2.13.") && Version(sv) <= Version(Constants.scala2Version) ||
                sv.startsWith("3.") && Version(sv) <= Version(Constants.scalaVersion)
              }
            }
            (targetId = id, recommended = recommended)
        }
      }
      retainedTargets.map {
        case (id, recommended) =>
          val targetOpt = targetMap.get(id).orElse {
            server.bspData.targetData(server0.info).flatMap(_.buildTargetInfo.get(id))
          }
          val name = targetOpt.map(_.getDisplayName).getOrElse {
            BspUtil.targetShortId(server.bspData, id)
          }

          val commentOpt = targetOpt.flatMap(_.asScalaBuildTarget).map { scalaTarget =>
            val sv = scalaTarget.getScalaVersion
            val platform = scalaTarget.getPlatform match {
              case b.ScalaPlatform.JVM    => "JVM"
              case b.ScalaPlatform.JS     => "Scala.js"
              case b.ScalaPlatform.NATIVE => "Scala Native"
            }
            // TODO Factor that somewhere
            val supported =
              sv.startsWith("2.13.") && Version(sv) <= Version(Constants.scala2Version) ||
              sv.startsWith("3.") && Version(sv) <= Version(Constants.scalaVersion)
            (supported, s"Scala $sv, $platform")
          }

          val supported = commentOpt.map(_._1).getOrElse(true)
          ModuleInfo(
            workspace = server0.info.workspace.toNIO.toUri.toASCIIString,
            server = server0.name,
            uri = id.getUri,
            label = (if (supported) if (recommended) "$(tag) " else "" else "$(warning) ") + name,
            detail = if (supported) "Load module" else "Unsupported Scala version",
            description = (Seq(server0.enhancedName) ++ commentOpt.map(_._2)).mkString(", "),
            alreadyLoaded = false
          )
      }
    }
  }

  /** Loads a single build target. Left on error, Right(false) when it was already loaded. */
  def loadModule(
    server: Server,
    indexer: Indexer,
    pools: ServerCommandThreadPools,
    workspace: os.Path,
    name: String,
    moduleUri: String
  ): Future[Either[String, Boolean]] = {
    val f = Future {
      server.bspServers.get(workspace, name) match {
        case Some(conn) =>
          val targetId = new b.BuildTargetIdentifier(moduleUri)
          val loaded   = indexer.addTarget(conn.info, targetId)
          if (loaded) {
            indexer.persist()
            indexer.reIndex().map(_ => Right(true))(using pools.dummyEc)
          }
          else {
            scribe.info(s"Module already added: $targetId")
            Future.successful(Right(false))
          }
        case None =>
          Future.successful(Left(s"No BSP server '$name' found under $workspace"))
      }
    }(using server.pools.requestsEces)
    f.flatten
  }

  /** Loads every build target every loaded build tool knows about, then indexes them. */
  def loadAllModules(
    server: Server,
    indexer: Indexer,
    toplevelCacheOnly: Boolean
  ): Future[Unit] = {
    val f = Future {
      val allTargetsByBuildServer =
        server.bspServers.list.flatMap(_._2).map { buildServer =>
          buildServer -> buildServer
            .conn
            .workspaceBuildTargets
            .get()
            .getTargets
            .asScala
            .toList
        }
      if (allTargetsByBuildServer.isEmpty)
        scribe.warn("No build servers found while loading all modules")
      indexer.targets = Map.empty
      for ((server, targets) <- allTargetsByBuildServer)
        indexer.addTargets(server.info, targets.map(_.getId))
      indexer.persist()
      indexer.index(
        toplevelCacheOnly = toplevelCacheOnly,
        ignoreToplevelSymbolsErrors = false,
        mayReadFromBspCache = false
      )
    }(using server.pools.requestsEces)
    f.flatten
  }

  /** Compiles the module a file belongs to. `None` when the file isn't in any loaded module. */
  def compile(
    server: Server,
    file: os.Path
  ): Future[Option[b.CompileResult]] = {
    val f = Future {
      server.compilations.compileFile(file) match {
        case None =>
          scribe.warn(s"No build target found for $file, nothing to compile")
          Future.successful(None)
        case Some(f) =>
          val recovered = f.recover {
            case ex
                if Iterator
                  .iterate(Option(ex))(_.flatMap(e => Option(e.getCause)))
                  .takeWhile(_.nonEmpty)
                  .flatten
                  .exists(e =>
                    e.isInstanceOf[ResponseErrorException] &&
                    Option(e.getMessage).exists(_.contains("Compilation failed"))
                  ) =>
              new b.CompileResult(b.StatusCode.ERROR)
          }(using server.pools.compilationEc)
          recovered.onComplete {
            case Success(_) =>
            case Failure(ex) =>
              scribe.error(s"Compiling $file failed", ex)
          }(using server.pools.compilationEc)
          recovered.map(Some(_))(using server.pools.dummyEc)
      }
    }(using server.pools.requestsEces)

    f.flatten
  }
}
