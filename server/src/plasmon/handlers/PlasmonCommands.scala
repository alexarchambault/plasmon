package plasmon.handlers

import ch.epfl.scala.{bsp4j => b}
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, writeToString}
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import com.google.gson.{Gson, JsonElement}
import plasmon.command.ServerCommandThreadPools
import plasmon.index.Indexer
import plasmon.jsonrpc.CommandHandler
import plasmon.jsonrpc.CommandHandler.ParamsHelpers.*
import plasmon.servercommand.BspUtil
import plasmon.bsp.{BuildServerInfo, ConnectionInfoJson}
import plasmon.jsonrpc.Handlers
import plasmon.Server

import java.net.URI
import java.util.concurrent.CompletableFuture

import scala.util.{Failure, Success}
import java.io.ByteArrayOutputStream
import java.io.PrintWriter
import java.nio.charset.StandardCharsets
import scala.build.bsp.WrappedSourcesParams
import java.util.concurrent.ExecutionException
import org.eclipse.lsp4j.jsonrpc.ResponseErrorException
import org.eclipse.lsp4j.jsonrpc.messages.ResponseErrorCode
import scala.build.bsp.WrappedSourcesResult
import scala.concurrent.Future
import java.io.File
import plasmon.Status
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import plasmon.bsp.BuildTool
import plasmon.pc.PresentationCompilers

import plasmon.PlasmonEnrichments._
import scala.jdk.CollectionConverters._
import plasmon.bsp.BspConnection
import scala.meta.pc.PresentationCompiler
import scala.meta.internal.pc.HasCompilerAccess
import org.eclipse.{lsp4j => l}
import scala.meta.pc.CancelToken
import plasmon.ide.CancelTokens
import scala.meta.internal.metals.EmptyCancelToken

object PlasmonCommands {

  private sealed abstract class BuildToolOrModule extends Product with Serializable

  private object BuildToolOrModule {
    final case class BuildTool(
      id: String,
      discoverId: String,
      label: String,
      detail: String,
      infos: Seq[ConnectionInfoJson],
      alreadyAdded: Boolean
    ) extends BuildToolOrModule
    final case class Module(
      workspace: String,
      server: String,
      uri: String,
      label: String,
      detail: String,
      alreadyLoaded: Boolean
    ) extends BuildToolOrModule

    implicit lazy val codec: JsonValueCodec[BuildToolOrModule] = JsonCodecMaker.make
    lazy val seqCodec: JsonValueCodec[Seq[BuildToolOrModule]]  = JsonCodecMaker.make
  }

  private final case class LoadBuildToolResult(
    success: Boolean,
    error: Option[String]
  )

  private object LoadBuildToolResult {
    implicit lazy val codec: JsonValueCodec[LoadBuildToolResult] = JsonCodecMaker.make
  }

  private final case class BuildToolRestartResponse(
    workspace: String,
    name: String,
    error: Option[String]
  )

  private object BuildToolRestartResponse {
    implicit lazy val codec: JsonValueCodec[BuildToolRestartResponse] = JsonCodecMaker.make
  }

  private final case class LoadModuleResponse(
    loaded: Boolean,
    error: String = ""
  )

  private object LoadModuleResponse {
    implicit lazy val codec: JsonValueCodec[LoadModuleResponse] = JsonCodecMaker.make
  }

  private final case class UnloadModuleResponse(
    unloaded: Boolean,
    error: String = ""
  )

  private object UnloadModuleResponse {
    implicit lazy val codec: JsonValueCodec[UnloadModuleResponse] = JsonCodecMaker.make
  }

  private final case class ModuleAction(
    label: String,
    detail: String,
    command: String,
    arguments: List[String]
  )

  private object ModuleAction {
    implicit lazy val codec: JsonValueCodec[ModuleAction] = JsonCodecMaker.make
    lazy val seqCodec: JsonValueCodec[Seq[ModuleAction]]  = JsonCodecMaker.make
  }

  private final case class CleanResponse(
    error: String,
    exceptionString: String
  )

  private object CleanResponse {
    implicit lazy val codec: JsonValueCodec[CleanResponse] = JsonCodecMaker.make
  }

  private final case class UnloadAllModulesResponse(
    moduleCount: Int
  )

  private object UnloadAllModulesResponse {
    implicit lazy val codec: JsonValueCodec[UnloadAllModulesResponse] = JsonCodecMaker.make
  }

  private final case class UnloadAllModulesAndBuildToolsResponse(
    moduleCount: Int,
    buildToolCount: Int
  )

  private object UnloadAllModulesAndBuildToolsResponse {
    implicit lazy val codec: JsonValueCodec[UnloadAllModulesAndBuildToolsResponse] =
      JsonCodecMaker.make
  }

  private final case class ReImportResponse(
    message: String = "",
    logId: Option[String] = None,
    error: String = ""
  )

  private object ReImportResponse {
    implicit lazy val codec: JsonValueCodec[ReImportResponse] = JsonCodecMaker.make
  }

  private final case class EnableFallbackCompilerResponse(
    enabled: Boolean
  )

  private object EnableFallbackCompilerResponse {
    implicit lazy val codec: JsonValueCodec[EnableFallbackCompilerResponse] = JsonCodecMaker.make
  }

  private final case class DisableFallbackCompilerResponse(
    disabled: Boolean
  )

  private object DisableFallbackCompilerResponse {
    implicit lazy val codec: JsonValueCodec[DisableFallbackCompilerResponse] = JsonCodecMaker.make
  }

  private final case class OrganizeImportsResponse(
    error: Option[String],
    fileChanged: Boolean
  )

  private object OrganizeImportsResponse {
    implicit lazy val codec: JsonValueCodec[OrganizeImportsResponse] = JsonCodecMaker.make
  }

  private final case class OrganizeImportsInModuleResponse(
    error: Option[String],
    fileChanged: Int
  )

  private object OrganizeImportsInModuleResponse {
    implicit lazy val codec: JsonValueCodec[OrganizeImportsInModuleResponse] = JsonCodecMaker.make
  }

  // quite ineffective, but does the job
  private def writeToGson[T: JsonValueCodec](t: T, gson: Gson = new Gson): JsonElement = {
    val str = writeToString(t)
    gson.fromJson(str, classOf[JsonElement])
  }

  private def discoverBuildTools(
    workspace: os.Path,
    fileOpt: Option[os.Path],
    alreadyAdded: Set[plasmon.bsp.BuildTool],
    tools: BuildTool.Tools
  ): Seq[BuildToolOrModule.BuildTool] = {
    val discovered           = BspUtil.discoverBuildTools(workspace, fileOpt)
    val (mayUnload, mayLoad) = discovered.partition(tool => alreadyAdded.contains(tool.buildTool))

    val mayLoadEntries = mayLoad.map { tool =>
      val infos =
        ConnectionInfoJson(
          tool.buildTool.launcher(tools).info
        ) +: tool.buildTool.extraLaunchers.map(t =>
          ConnectionInfoJson(t.info)
        )
      BuildToolOrModule.BuildTool(
        tool.buildTool.id,
        tool.discoverId,
        tool.buildTool.description(workspace),
        "Load build tool" + tool.warning.fold("")(msg => s" (warning: $msg)"),
        infos,
        alreadyAdded = false
      )
    }
    val mayUnloadEntries = mayUnload.map { tool =>
      val infos =
        ConnectionInfoJson(
          tool.buildTool.launcher(tools).info
        ) +: tool.buildTool.extraLaunchers.map(t =>
          ConnectionInfoJson(t.info)
        )
      BuildToolOrModule.BuildTool(
        tool.buildTool.id,
        tool.discoverId,
        tool.buildTool.description(workspace),
        "Unload build tool" + tool.warning.fold("")(msg => s" (warning: $msg)"),
        infos,
        alreadyAdded = true
      )
    }

    mayLoadEntries ++ mayUnloadEntries
  }

  private def listModules(
    workspace: os.Path,
    fileOpt: Option[os.Path],
    server: Server
  ): Seq[BuildToolOrModule.Module] =
    fileOpt.toSeq.flatMap { file =>
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
              if (server0.info.onlyTargets.forall(_.contains(targetId.getUri))) {
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
                Nil
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
        targets0.collect {
          case id if server0.info.onlyTargets.forall(_.contains(id.getUri)) =>
            val name = targetMap.get(id).map(_.getDisplayName).getOrElse {
              val targetOpt =
                server.bspData.targetData(server0.info).flatMap(_.buildTargetInfo.get(id))
              targetOpt.fold(BspUtil.targetShortId(server.bspData, id))(_.getDisplayName)
            }
            val alreadyLoaded = loadedTargetIds.contains(id)
            BuildToolOrModule.Module(
              server0.info.workspace.toNIO.toUri.toASCIIString,
              server0.name,
              id.getUri,
              name,
              if (alreadyLoaded) "Unload module" else "Load module",
              alreadyLoaded
            )
        }
      }
    }

  def restartBuildTool(
    server: Server,
    pools: ServerCommandThreadPools,
    buildTool: plasmon.bsp.BuildTool,
    info: BuildServerInfo,
    name: String,
    hard: Boolean
  ): Unit = {
    val connOpt = server.bspServers.get(info)
    if (connOpt.isEmpty)
      scribe.error(s"No connection details found for $info (should not happen)")
    server.bspServers.remove(info, hard)
    val targetDataOpt = server.bspData.targetData(info)
    if (targetDataOpt.isEmpty)
      scribe.error(s"No target data found for $info (should not happen)")
    server.bspServers.addOne(
      buildTool,
      info,
      name,
      str => scribe.info(s"Starting BSP server: $str"), // FIXME Show that to users too
      pools.bspEces,
      () => pools.bloopThreads
    )
    val updatedConnOpt = server.bspServers.get(info)
    if (updatedConnOpt.isEmpty)
      scribe.error(s"No connection details found for $info after restart (should not happen)")
    for (data <- targetDataOpt)
      data.buildServerOpt = connOpt.map(_.conn)
  }

  def buildToolRestartCommand(
    server: Server,
    indexer: Indexer,
    pools: ServerCommandThreadPools,
    commandName: String,
    hard: Boolean
  ) =
    CommandHandler.of(commandName, refreshStatus = true) { (params, logger) =>
      params.as[String](commandName) { uri0 =>
        val path           = uri0.osPathFromUri
        val buildTargetOpt = server.bspData.inverseSources(path)
        val bspServerOpt = buildTargetOpt.toRight(s"No module found for $path")
          .flatMap(id =>
            server.bspData.buildServerOf(id).map((id, _)).toRight(
              s"No build server found for module $id (internal consistency error, should not happen)"
            )
          )
          .flatMap {
            case (id, conn) =>
              server.bspServers
                .list
                .iterator
                .flatMap {
                  case (buildTool, conns) =>
                    conns.iterator.map((buildTool, _))
                }
                .find(_._2.conn == conn)
                .toRight(
                  s"No build tool found for module $id (internal consistency error, should not happen)"
                )
          }
        val restarted = bspServerOpt.map {
          case (buildTool, bspServer) =>
            restartBuildTool(server, pools, buildTool, bspServer.info, bspServer.name, hard)
            indexer.reIndex().onComplete {
              case Success(()) =>
              case Failure(ex) =>
                scribe.error("Error re-indexing", ex)
            }(pools.dummyEc)
            ()
        }
        val resp = BuildToolRestartResponse(
          bspServerOpt.toOption.fold("")(_._2.info.workspace.toString),
          bspServerOpt.toOption.fold("")(_._2.name),
          restarted.left.toOption
        )
        CompletableFuture.completedFuture(writeToGson(resp))
      }
    }

  def buildToolsCommands(server: Server, indexer: Indexer, pools: ServerCommandThreadPools) = Seq(
    CommandHandler.of("plasmon/listBuildTools") { (params, logger) =>
      params.asOpt[String]("plasmon/listBuildTools") { fileOpt0 =>
        val workspace = server.workspace()
        val fileOpt   = fileOpt0.map(_.osPathFromUri)
        val buildTools: Seq[BuildToolOrModule] =
          discoverBuildTools(
            workspace,
            fileOpt,
            server.bspServers.list.map(_._1).toSet,
            server.tools
          )
        val resp = writeToGson(buildTools)(BuildToolOrModule.seqCodec)
        CompletableFuture.completedFuture(resp)
      }
    },
    CommandHandler.of("plasmon/listBuildToolsOrModules") { (params, logger) =>
      params.asOpt[String]("plasmon/listBuildToolsOrModules") { fileOpt0 =>
        val workspace = os.Path(server.workspace().toNIO)
        val fileOpt   = fileOpt0.map(_.osPathFromUri)
        val buildTools =
          discoverBuildTools(
            workspace,
            fileOpt,
            server.bspServers.list.map(_._1).toSet,
            server.tools
          )
        val modules = listModules(workspace, fileOpt, server)
        val resp    = writeToGson(buildTools ++ modules)(BuildToolOrModule.seqCodec)
        CompletableFuture.completedFuture(resp)
      }
    },
    CommandHandler.of("plasmon/loadBuildTool", refreshStatus = true) { (params, logger) =>

      def load(
        discoverId: String,
        toolId: String,
        currentFileOpt: Option[os.Path]
      ): CompletableFuture[Object] = {
        val res = BspUtil.BuildToolDiscover.map.get(discoverId) match {
          case Some(discover) =>
            val tools = discover.check(server.workspace(), currentFileOpt)
            tools.find(_.buildTool.id == toolId) match {
              case Some(tool) =>
                val res = server.bspServers.tryAdd(
                  tool.buildTool,
                  tool.buildTool.launcher(server.tools) +: tool.buildTool.extraLaunchers,
                  line => scribe.info("BSP: " + line),
                  pools.bspEces,
                  () => pools.bloopThreads
                )
                Await.result(res, Duration.Inf) match {
                  case Left(err) => LoadBuildToolResult(success = false, Some(err))
                  case Right(()) =>
                    server.bspServers.persist()
                    LoadBuildToolResult(success = true, None)
                }
              case None =>
                val found = tools.map(tool => (tool.buildTool.id, tool.discoverId))
                scribe.error(s"Build tool $toolId / $discoverId not found, available tools: $found")
                scribe.error(s"currentFileOpt=$currentFileOpt")
                LoadBuildToolResult(
                  success = false,
                  Some(s"Build tool $toolId / $discoverId not found (internal error)")
                )
            }
          case None =>
            LoadBuildToolResult(success = false, Some(s"Build tool $discoverId not found"))
        }

        CompletableFuture.completedFuture(writeToGson(res))
      }

      if (params.arguments.length == 2)
        params.asValues[String, String]("plasmon/loadBuildTool") { (discoverId, toolId) =>
          load(discoverId, toolId, server.editorState.focusedDocument)
        }
      else
        params.asValues[String, String, String]("plasmon/loadBuildTool") {
          (discoverId, toolId, strPath) =>
            load(discoverId, toolId, Some(strPath.osPathFromUri))
        }
    },
    CommandHandler.of("plasmon/unloadBuildTool", refreshStatus = true) { (params, logger) =>

      def unload(
        discoverId: String,
        toolId: String,
        currentFileOpt: Option[os.Path]
      ): CompletableFuture[Object] = {
        val res = BspUtil.BuildToolDiscover.map.get(discoverId) match {
          case Some(discover) =>
            val tools = discover.check(server.workspace(), currentFileOpt)
            tools.find(_.buildTool.id == toolId) match {
              case Some(tool) =>
                val allInfo =
                  tool.buildTool.launcher(server.tools).info +: tool.buildTool.extraLaunchers.map(
                    _.info
                  )
                val results = allInfo.map { info =>
                  val removedFromIndexer = indexer.dontReloadBuildTool(info)
                  val removedFromServerList =
                    Await.result(server.bspServers.remove(info), Duration.Inf).nonEmpty
                  if (!removedFromIndexer)
                    scribe.warn(s"Build tool $info wasn't removed from indexer")
                  if (!removedFromServerList)
                    scribe.warn(s"Build tool $info wasn't removed from server list")
                  (removedFromIndexer, removedFromServerList)
                }
                if (results.exists(_._1))
                  indexer.persist()
                if (results.exists(_._2))
                  server.bspServers.persist()
                if (allInfo.exists(info => server.bspData.targetData(info).isDefined))
                  indexer.reIndex().onComplete {
                    case Success(()) =>
                    case Failure(ex) =>
                      scribe.error("Error re-indexing", ex)
                  }(pools.dummyEc)
                // FIXME We don't really check that everything went right here
                LoadBuildToolResult(success = true, None)
              case None =>
                LoadBuildToolResult(
                  success = false,
                  Some(s"Build tool $toolId not found (internal error)")
                )
            }
          case None =>
            LoadBuildToolResult(success = false, Some(s"Build tool $toolId not found"))
        }

        CompletableFuture.completedFuture(writeToGson(res))
      }

      if (params.arguments.length == 2)
        params.asValues[String, String]("plasmon/unloadBuildTool") { (discoverId, toolId) =>
          unload(discoverId, toolId, None)
        }
      else
        params.asValues[String, String, String]("plasmon/unloadBuildTool") {
          (discoverId, toolId, strPath) =>
            unload(discoverId, toolId, Some(strPath.osPathFromUri))
        }
    },
    buildToolRestartCommand(server, indexer, pools, "plasmon/buildToolRestart", hard = true),
    buildToolRestartCommand(
      server,
      indexer,
      pools,
      "plasmon/buildToolRestartOrReconnect",
      hard = false
    ),
    CommandHandler.of("plasmon/unloadAllModules", refreshStatus = true) { (_, _) =>
      val removedCount =
        indexer.targets
          .map {
            case (info, targetIds) =>
              for (id <- targetIds)
                indexer.removeTarget(info, id)
              targetIds.length
          }
          .sum +
          indexer.addAllTargets
            .map { info =>
              val count = server.bspData.targetData(info).map(_.targetToWorkspace.size).getOrElse(0)
              indexer.removeAllTargets(info)
              count
            }
            .sum
      if (removedCount > 0) {
        indexer.persist()
        indexer.reIndex().onComplete {
          case Success(()) =>
          case Failure(ex) =>
            scribe.error("Error re-indexing", ex)
        }(pools.dummyEc)
      }
      CompletableFuture.completedFuture(writeToGson(UnloadAllModulesResponse(removedCount)))
    },
    CommandHandler.of("plasmon/unloadAllBuildTools", refreshStatus = true) { (_, _) =>
      val allInfos = indexer.targets.keySet ++ indexer.addAllTargets
      val removed =
        indexer.targets.map {
          case (info, targetIds) =>
            indexer.dontReloadBuildTool(info)
            targetIds.length
        } ++
          indexer.addAllTargets.map { info =>
            val count = server.bspData.targetData(info).map(_.targetToWorkspace.size).getOrElse(0)
            indexer.removeAllTargets(info)
            count
          }
      val removedBuildToolCount = removed.size
      val removedModuleCount    = removed.sum
      if (allInfos.nonEmpty) {
        for (info <- allInfos)
          server.bspServers.remove(info)
        indexer.persist()
        server.bspServers.persist()
        indexer.reIndex().onComplete {
          case Success(()) =>
          case Failure(ex) =>
            scribe.error("Error re-indexing", ex)
        }(pools.dummyEc)
      }
      CompletableFuture.completedFuture(
        writeToGson(
          UnloadAllModulesAndBuildToolsResponse(
            removedModuleCount,
            removedBuildToolCount
          )
        )
      )
    },
    CommandHandler.of("plasmon/compile", refreshStatus = true) { (params, logger) =>
      params.asFileUri("plasmon/compile") { file =>
        server.compilations.compileFile(file) match {
          case None =>
            scribe.warn(s"No build target found for $file, nothing to compile")
            CompletableFuture.completedFuture[Object](null)
          case Some(f) =>
            f.onComplete {
              case Success(_) =>
              case Failure(ex) =>
                scribe.error(s"Compiling $file failed", ex)
            }(server.pools.compilationEc)
            f.map[Object](_ => null)(server.pools.dummyEc).asJava
        }
      }
    },
    CommandHandler.of("plasmon/clean", refreshStatus = true) { (params, logger) =>
      params.asFileUri("plasmon/clean") { file =>
        val f: CompletableFuture[CleanResponse] =
          server.bspData.inverseSources(file) match {
            case Some(targetId) =>
              server.bspData.buildServerOf(targetId) match {
                case Some(buildServer) =>
                  buildServer
                    .buildTargetCleanCache(new b.CleanCacheParams(List(targetId).asJava))
                    .handle {
                      (resOrNull, exOrNull) =>
                        if (resOrNull != null)
                          CleanResponse("", "")
                        else if (exOrNull != null) {
                          scribe.error(s"Error while cleaning $targetId", exOrNull)
                          val baos = new ByteArrayOutputStream
                          exOrNull.printStackTrace(new PrintWriter(
                            baos,
                            true,
                            StandardCharsets.UTF_8
                          ))
                          CleanResponse(
                            exOrNull.toString,
                            new String(baos.toByteArray, StandardCharsets.UTF_8)
                          )
                        }
                        else {
                          // should not happen?
                          scribe.warn("No result or error in buildTargetCleanCache response")
                          CleanResponse("", "")
                        }
                    }
                case None =>
                  scribe.warn(s"No build server found for target $targetId")
                  CompletableFuture.completedFuture(
                    CleanResponse(s"Internal error: no build server found for target $targetId", "")
                  )
              }
            case None =>
              scribe.warn(s"No build target found for $file")
              CompletableFuture.completedFuture(
                CleanResponse(s"No build target found for $file", "")
              )
          }
        f.thenApply { res =>
          server.refreshStatus()
          writeToGson(res): Object
        }
      }
    },
    CommandHandler.of("plasmon/compileAll", refreshStatus = true) { (_, _) =>
      val f = server.compilations.compileFiles(server.editorState.buffers.open.toSeq)
      f.onComplete {
        case Success(_) =>
        case Failure(ex) =>
          scribe.error("Compiling all files failed", ex)
      }(server.pools.compilationEc)
      f.map[Object](_ => null)(server.pools.dummyEc).asJava
    },
    CommandHandler.of("plasmon/cleanAll", refreshStatus = true) { (_, _) =>
      ???
    },
    CommandHandler.of("plasmon/reImport", refreshStatus = true) { (params, logger) =>
      params.asFileUri("plasmon/reImport") { file =>
        val resp = buildServer(server, file) match {
          case Left(error) =>
            ReImportResponse(error = error)
          case Right((buildTool, conn, _)) =>
            val prepareSteps = conn.info.prepare.toSeq ++ conn.launcher.prepare.toSeq
            if (prepareSteps.isEmpty)
              ReImportResponse(s"${conn.name} has no import step")
            else {
              val f = Future {
                scribe.info(s"Re-importing build ${conn.name} for $file")
                prepareSteps.foreach(_(conn.logger, true))
                restartBuildTool(server, pools, buildTool, conn.info, conn.name, hard = false)
              }(pools.bspEces) // use another pool?
              f.onComplete {
                case Success(()) =>
                  indexer.reIndex().onComplete {
                    case Success(()) =>
                    case Failure(ex) =>
                      scribe.error("Error re-indexing", ex)
                  }(pools.dummyEc)
                  scribe.info(s"Done re-importing build ${conn.name} for $file")
                case Failure(ex) =>
                  scribe.error(s"Error re-importing build ${conn.name} for $file", ex)
                  conn.logger.log("Error re-importing build")
                  conn.logger.log(ex)
              }(pools.bspEces)
              ReImportResponse(s"Re-importing ${conn.name}", Some(conn.logger.channel.id))
            }
        }
        CompletableFuture.completedFuture(writeToGson(resp))
      }
    },
    CommandHandler.of("plasmon/organizeImports", refreshStatus = true) { (params, logger) =>
      params.asFileUri("plasmon/organizeImports") { file =>
        val resp = buildServer(server, file) match {
          case Right((_, conn, targetId)) =>
            val semdbStatus = server.status.semdbStatus(targetId, file)
            semdbStatus match {
              case Status.SemdbStatus.Stale =>
                OrganizeImportsResponse(error = Some("Stale semantic db"), fileChanged = false)
              case Status.SemdbStatus.NotFound =>
                OrganizeImportsResponse(error = Some("Semantic db not found"), fileChanged = false)
              case Status.SemdbStatus.UpToDate =>
                server.bspData.targetData(conn.info) match {
                  case Some(data) =>
                    // TODO scalafix config
                    val cp = data.scalaTarget(targetId).flatMap(_.classpath).getOrElse(Nil)
                    val scalacOptions = {
                      val baseOptions = data.scalaTarget(targetId).map(_.options).getOrElse(Nil)
                      val is213 =
                        data.scalaTarget(targetId).exists(_.scalaVersion.startsWith("2.13."))
                      val unusedOpt = if (is213) "-Wunused:imports" else "-Ywarn-unused-import"
                      if (baseOptions.contains(unusedOpt)) baseOptions
                      else unusedOpt :: baseOptions
                    }
                    val sourceRoot = server.workingDir // FIXME
                    val logger     = server.loggerManager.create("scalafix", "Scalafix")
                    val res = logger.logCommand(os.proc(
                      "scalafix-native",
                      "--classpath",
                      cp.map(_.toString).mkString(File.pathSeparator),
                      "--sourceroot",
                      sourceRoot,
                      "-f",
                      file,
                      scalacOptions.flatMap(opt => List("--scalac-options", opt)),
                      "-r",
                      "OrganizeImports"
                    ))
                      .call(
                        cwd = server.workingDir,
                        stderr = logger.processOutput,
                        check = false
                      )
                    if (res.exitCode == 0) {
                      val patchStr = res.out.text()
                      val patch =
                        difflib.DiffUtils.parseUnifiedDiff(patchStr.linesIterator.toList.asJava)
                      val content = os.read(file)
                      val lineSep = content.linesIterator.zip(content.linesWithSeparators)
                        .map {
                          case (line, lineWithSep) =>
                            lineWithSep.stripPrefix(line)
                        }
                        .find(_.nonEmpty)
                        .getOrElse(System.lineSeparator())
                      val updatedContent =
                        patch.applyTo(content.linesIterator.toVector.asJava).asScala
                          .iterator
                          .map(_ + lineSep)
                          .mkString // FIXME Preserve missing line sep at end of file?
                      val fileChanged =
                        if (content == updatedContent) false
                        else {
                          os.write.over(file, updatedContent.getBytes(StandardCharsets.UTF_8))
                          true
                        }
                      OrganizeImportsResponse(error = None, fileChanged = fileChanged)
                    }
                    else
                      // TODO Pass command to open log
                      OrganizeImportsResponse(
                        error = Some("Error running scalafix-native"),
                        fileChanged = false
                      )
                  case None =>
                    OrganizeImportsResponse(
                      error = Some(s"Internal error (no target data found for ${conn.name})"),
                      fileChanged = false
                    )
                }
            }
          case Left(error) =>
            OrganizeImportsResponse(error = Some(error), fileChanged = false)
        }
        CompletableFuture.completedFuture(writeToGson(resp))
      }
    },
    CommandHandler.of("plasmon/organizeImportsInModule", refreshStatus = true) { (params, logger) =>
      params.asFileUri("plasmon/organizeImportsInModule") { _ =>
        ???
        CompletableFuture.completedFuture(writeToGson(OrganizeImportsInModuleResponse(???, ???)))
      }
    }
  )

  def listModulesCommands(server: Server, indexer: Indexer, pools: ServerCommandThreadPools) = Seq(
    CommandHandler.of("plasmon/listModuleActions") { (params, logger) =>
      params.asOpt[String]("plasmon/listModuleActions") { fileOpt0 =>
        val workspace = server.workspace()
        val fileOpt = fileOpt0.map(_.osPathFromUri)
          .orElse(server.editorState.focusedDocument)

        val actions = fileOpt match {
          case Some(file) =>
            server.bspData.inverseSources(file) match {
              case Some(buildTarget) =>
                val hasPastOrOnGoingCompilation =
                  server.compilations.latestCompilations.get(buildTarget).exists {
                    case (pastCompilationOpt, onGoingCompilationOpt) =>
                      pastCompilationOpt.isDefined || onGoingCompilationOpt.isDefined
                  }
                val compileOrReCompileLabel =
                  if (hasPastOrOnGoingCompilation) "Re-compile"
                  else "Compile"
                Seq(
                  ModuleAction(
                    label = "Re-index",
                    detail = "Re-index loaded modules",
                    command = "plasmon/index",
                    arguments = Nil
                  ),
                  ModuleAction(
                    label = compileOrReCompileLabel,
                    detail =
                      s"$compileOrReCompileLabel ${BspUtil.targetShortId(server.bspData, buildTarget)} module",
                    command = "plasmon/compile",
                    arguments = List(file.toNIO.toUri.toASCIIString)
                  ),
                  ModuleAction(
                    label = "Clean",
                    detail = s"Clean ${BspUtil.targetShortId(server.bspData, buildTarget)} module",
                    command = "plasmon/clean",
                    arguments = List(file.toNIO.toUri.toASCIIString)
                  )
                )
              case None =>
                scribe.info(s"plasmon/listModuleActions: No build target found for $file")
                Nil
            }
          case None =>
            scribe.info(
              "plasmon/listModuleActions: no file passed, and focused document is unknown"
            )
            Nil
        }

        CompletableFuture.completedFuture(writeToGson(actions)(ModuleAction.seqCodec))
      }
    },
    CommandHandler.of("plasmon/loadModule", refreshStatus = true) { (params, logger) =>
      params.asValues[String, String, String]("plasmon/loadModule") {
        (workspaceUri, name, moduleUri) =>
          val workspace = workspaceUri.osPathFromUri
          val infoOpt   = server.bspServers.get(workspace, name)
          val resp = infoOpt match {
            case Some(conn) =>
              val targetId = new b.BuildTargetIdentifier(moduleUri)
              val loaded   = indexer.addTarget(conn.info, targetId)
              if (loaded) {
                indexer.persist()
                indexer.reIndex().onComplete {
                  case Success(()) =>
                  case Failure(ex) =>
                    scribe.error("Error re-indexing", ex)
                }(pools.dummyEc)
              }
              else
                scribe.info(s"Module already added: $targetId")
              LoadModuleResponse(loaded)
            case None =>
              LoadModuleResponse(
                loaded = false,
                error = s"No BSP server '$name' found under $workspace"
              )
          }
          CompletableFuture.completedFuture(writeToGson(resp))
      }
    },
    CommandHandler.of("plasmon/unloadModule", refreshStatus = true) { (params, logger) =>
      params.asValues[String, String, String]("plasmon/unloadModule") {
        (workspaceUri, name, moduleUri) =>
          val workspace = workspaceUri.osPathFromUri
          val infoOpt   = server.bspServers.get(workspace, name)
          val resp = infoOpt match {
            case Some(conn) =>
              val targetId = new b.BuildTargetIdentifier(moduleUri)
              val unloaded = indexer.removeTarget(conn.info, targetId)
              if (unloaded) {
                indexer.persist()
                indexer.reIndex().onComplete {
                  case Success(()) =>
                  case Failure(ex) =>
                    scribe.error("Error re-indexing", ex)
                }(pools.dummyEc)
              }
              else
                scribe.info(s"Module wasn't loaded: $targetId")
              UnloadModuleResponse(unloaded)
            case None =>
              UnloadModuleResponse(
                unloaded = false,
                error = s"No BSP server '$name' found under $workspace"
              )
          }
          CompletableFuture.completedFuture(writeToGson(resp))
      }
    }
  )

  private final case class Action(
    command: String,
    commandArgs: Seq[String],
    text: String,
    id: String,
    description: String = null
  )

  private object Action {
    implicit lazy val codec: JsonValueCodec[Action] = JsonCodecMaker.make
    lazy val seqCodec: JsonValueCodec[Seq[Action]]  = JsonCodecMaker.make
  }

  def reindexCommands(server: Server, indexer: Indexer, pools: ServerCommandThreadPools) =
    Seq(
      CommandHandler.of("plasmon/reIndexActions") { (params, logger) =>
        params.as[String]("plasmon/reIndexActions") { strUri =>
          val file        = strUri.osPathFromUri
          val targetIdOpt = server.bspData.inverseSources(file)
          val actions =
            if (targetIdOpt.isDefined)
              Seq(
                Action(
                  "plasmon/index",
                  Nil,
                  "Re-index project",
                  "reindex"
                )
              )
            else
              Nil
          val elem = writeToGson(actions)(Action.seqCodec)
          CompletableFuture.completedFuture(elem)
        }
      },
      CommandHandler.of("plasmon/index", refreshStatus = true) { (_, _) =>
        indexer.reIndex().onComplete {
          case Success(()) =>
          case Failure(ex) =>
            scribe.error("Error re-indexing", ex)
        }(pools.dummyEc)
        CompletableFuture.completedFuture(null)
      }
    )

  private final case class InteractiveCompilerStartResponse(
    started: Boolean
  )

  private object InteractiveCompilerStartResponse {
    implicit lazy val codec: JsonValueCodec[InteractiveCompilerStartResponse] = JsonCodecMaker.make
  }

  private final case class InteractiveCompilerRemoveResponse(
    removed: Boolean
  )

  private object InteractiveCompilerRemoveResponse {
    implicit lazy val codec: JsonValueCodec[InteractiveCompilerRemoveResponse] = JsonCodecMaker.make
  }

  private final case class InteractiveCompilerInterruptResponse(
    interrupted: Boolean
  )

  private object InteractiveCompilerInterruptResponse {
    implicit lazy val codec: JsonValueCodec[InteractiveCompilerInterruptResponse] =
      JsonCodecMaker.make
  }

  private def presentationCompilerKey(server: Server, targetOrPath: String) =
    if (targetOrPath.startsWith("target:")) {
      val targetId = targetOrPath.stripPrefix("target:")
      Some(PresentationCompilers.PresentationCompilerKey.ScalaBuildTarget(
        new b.BuildTargetIdentifier(targetId)
      ))
    }
    else if (targetOrPath.startsWith("path:")) {
      val path = targetOrPath.stripPrefix("path:").osPathFromUri
      server.bspData.inverseSources(path)
        .map(PresentationCompilers.PresentationCompilerKey.ScalaBuildTarget(_))
    }
    else
      None

  def interactiveCompilerCommands(server: Server) =
    Seq(
      CommandHandler.of("plasmon/interactiveCompilerActions") { (params, logger) =>
        scribe.info("params=" + pprint.apply(params))
        params.asFileUri("plasmon/interactiveCompilerActions") { file =>
          val targetIdOpt = server.bspData.inverseSources(file)

          val actions = targetIdOpt match {
            case None =>
              Nil
            case Some(targetId) =>
              val key    = PresentationCompilers.PresentationCompilerKey.ScalaBuildTarget(targetId)
              val keyStr = targetId.getUri

              val hasMainCompiler = server.presentationCompilers.jcache.containsKey(key)
              val hasCompletionCompiler =
                server.presentationCompilers.jCompletionCache.containsKey(key)
              def noMainActions = Seq(
                Action(
                  "plasmon/interactiveCompilerStart",
                  Seq(s"target:$keyStr", ""),
                  "Start interactive compiler",
                  "start"
                )
              )
              def mainActions = Seq(
                Action(
                  "plasmon/interactiveCompilerInterrupt",
                  Seq(s"target:$keyStr", ""),
                  "Interrupt interactive compiler",
                  "interrupt",
                  "Send interruption to interactive compiler thread"
                ),
                Action(
                  "plasmon/interactiveCompilerRemove",
                  Seq(s"target:$keyStr", ""),
                  "Shutdown interactive compiler",
                  "shutdown",
                  "A new one will then be created on demand when needed"
                )
              )
              def noCompletionActions = Seq(
                Action(
                  "plasmon/interactiveCompilerStart",
                  Seq(s"target:$keyStr", "completion"),
                  "Start interactive compiler (completions)",
                  "start-completion"
                )
              )
              def completionActions = Seq(
                Action(
                  "plasmon/interactiveCompilerInterrupt",
                  Seq(s"target:$keyStr", "completion"),
                  "Interrupt interactive compiler for completions",
                  "interrupt-completion",
                  "Send interruption to interactive compiler thread"
                ),
                Action(
                  "plasmon/interactiveCompilerRemove",
                  Seq(s"target:$keyStr", "completion"),
                  "Shutdown interactive compiler for completions",
                  "shutdown-completion",
                  "A new one will then be created on demand when needed"
                )
              )
              (if (hasMainCompiler) mainActions else noMainActions) ++
                (if (hasCompletionCompiler) completionActions else noCompletionActions)
          }
          val elem = writeToGson(actions)(Action.seqCodec)
          CompletableFuture.completedFuture(elem)
        }
      },
      CommandHandler.of("plasmon/interactiveCompilerStart", refreshStatus = true) {
        (params, logger) =>
          params.asValues[String, String]("plasmon/interactiveCompilerStart") {
            (targetOrPath, compilerId) =>
              val key = presentationCompilerKey(server, targetOrPath).getOrElse {
                sys.error(
                  s"Malformed target or path argument: '$targetOrPath' (expected path:… or target:…)"
                )
              }
              val isCompletion = compilerId == "completion"
              val dict =
                if (isCompletion) server.presentationCompilers.jCompletionCache
                else server.presentationCompilers.jcache
              val newCompiler =
                if (dict.containsKey(key)) None
                else
                  key match {
                    case t: PresentationCompilers.PresentationCompilerKey.ScalaBuildTarget =>
                      server.presentationCompilers.loadCompiler(t.id, isCompletion)
                  }
              if (newCompiler.isDefined)
                scribe.info(
                  s"Created interactive compiler for $key (isCompletion: $isCompletion)"
                )
              else
                scribe.info(
                  s"No interactive compiler created for $key (isCompletion: $isCompletion)"
                )
              val resp = writeToGson(InteractiveCompilerStartResponse(newCompiler.isDefined))
              CompletableFuture.completedFuture(resp)
          }
      },
      CommandHandler.of("plasmon/interactiveCompilerRemove", refreshStatus = true) {
        (params, logger) =>
          params.asValues[String, String]("plasmon/interactiveCompilerRemove") {
            (targetOrPath, compilerId) =>
              val key = presentationCompilerKey(server, targetOrPath).getOrElse {
                sys.error(
                  s"Malformed target or path argument: '$targetOrPath' (expected path:… or target:…)"
                )
              }
              val isCompletion = compilerId == "completion"
              val dict =
                if (isCompletion) server.presentationCompilers.jCompletionCache
                else server.presentationCompilers.jcache
              val removedOrNull = dict.remove(key)
              if (removedOrNull == null) {
                scribe.info(
                  s"No interactive compiler to remove for $key (isCompletion: $isCompletion)"
                )
                scribe.info(s"To remove: $key")
                scribe.info(s"Keys: ${dict.keySet().asScala.toVector.map(_.toString).sorted}")
              }
              else {
                removedOrNull.shutdown()
                scribe.info(
                  s"Removed interactive compiler for $key (isCompletion: $isCompletion)"
                )
              }
              val resp = writeToGson(InteractiveCompilerRemoveResponse(removedOrNull != null))
              CompletableFuture.completedFuture(resp)
          }
      },
      CommandHandler.of("plasmon/interactiveCompilerInterrupt", refreshStatus = true) {
        (params, logger) =>
          params.asValues[String, String]("plasmon/interactiveCompilerInterrupt") {
            (targetOrPath, compilerId) =>
              val key = presentationCompilerKey(server, targetOrPath).getOrElse {
                sys.error(
                  s"Malformed target or path argument: '$targetOrPath' (expected path:… or target:…)"
                )
              }
              val isCompletion = compilerId == "completion"
              val dict =
                if (isCompletion) server.presentationCompilers.jCompletionCache
                else server.presentationCompilers.jcache
              val compilerOrNull = dict.get(key)
              val interrupted =
                if (compilerOrNull == null) {
                  scribe.info(
                    s"No interactive compiler to found for $key (isCompletion: $isCompletion)"
                  )
                  false
                }
                else {
                  scribe.info(
                    s"Interrupting interactive compiler for $key (isCompletion: $isCompletion)"
                  )
                  val interrupted0: Boolean = compilerOrNull.compilerOpt
                    .collect {
                      case pc: PresentationCompiler with HasCompilerAccess =>
                        pc
                    }
                    .exists(_.compilerAccess.interrupt())
                  if (interrupted0)
                    scribe.info(
                      s"Interrupting interactive compiler for $key (isCompletion: $isCompletion): interrupted"
                    )
                  else
                    scribe.info(
                      s"Interrupting interactive compiler for $key (isCompletion: $isCompletion): nothing was interrupted"
                    )
                  interrupted0
                }
              val resp = writeToGson(InteractiveCompilerInterruptResponse(interrupted))
              CompletableFuture.completedFuture(resp)
          }
      }
    )

  private def buildServer(server: Server, file: os.Path)
    : Either[String, (BuildTool, BspConnection, b.BuildTargetIdentifier)] =
    server.bspData.inverseSources(file) match {
      case Some(targetId) =>
        server.bspData.buildServerOf(targetId) match {
          case Some(buildServer) =>
            val connOpt = server.bspServers
              .list
              .iterator
              .flatMap(t => t._2.iterator.map((t._1, _, targetId)))
              .find(_._2.conn == buildServer)
            connOpt.toRight {
              scribe.error(
                s"No BSP server found for $buildServer, target ${targetId.getUri} (BSP servers: ${server.bspServers.list})"
              )
              "Internal error"
            }
          case None =>
            scribe.error(
              s"No BSP server found for target ${targetId.getUri} (${server.bspData.allWritableData.map(_.targetToWorkspace)})"
            )
            Left("Internal error")
        }
      case None =>
        Left(s"$file is not loaded")
    }

  private final case class DebugFullTreeResp(
    targetId: String,
    fullTree: String,
    diagnostics: String,
    error: String
  )
  private object DebugFullTreeResp {
    implicit lazy val codec: JsonValueCodec[DebugFullTreeResp] =
      JsonCodecMaker.make
  }

  private final case class DebugBspDataResp(
    targetId: String,
    data: String,
    error: String
  )
  private object DebugBspDataResp {
    implicit lazy val codec: JsonValueCodec[DebugBspDataResp] =
      JsonCodecMaker.make
  }

  private final case class DebugSemanticdbLookupResp(
    targetId: String,
    text: String,
    error: String
  )
  private object DebugSemanticdbLookupResp {
    implicit lazy val codec: JsonValueCodec[DebugSemanticdbLookupResp] =
      JsonCodecMaker.make
  }

  private final case class DebugSymbolIndexResp(
    targetId: String,
    content: String,
    error: String
  )
  private object DebugSymbolIndexResp {
    implicit lazy val codec: JsonValueCodec[DebugSymbolIndexResp] =
      JsonCodecMaker.make
  }

  private final case class DebugPcResp(
    enabled: Boolean,
    changed: Boolean,
    logPath: String
  )
  private object DebugPcResp {
    implicit lazy val codec: JsonValueCodec[DebugPcResp] =
      JsonCodecMaker.make
  }

  def debugCommands(server: Server) =
    Seq(
      // TODO Merge debugSymbolIndex and debugFullTree?
      CommandHandler.of("plasmon/debugSymbolIndex") { (params, logger) =>
        params.asFileUri("plasmon/debugSymbolIndex") { file =>
          val buildTargetOpt = server.bspData.inverseSources(file)
          val contentOrError =
            buildTargetOpt.toRight(s"No build target found for $file").map { targetId =>
              val content = server.symbolIndex.dialectBuckets
                .iterator
                .collect {
                  case ((dialectOpt, mod), bucket) if mod.targetId == targetId.getUri =>
                    (dialectOpt, bucket)
                }
                .toVector
                .sortBy(_._1.map(_.toString).getOrElse(""))
                .map {
                  case (dialectOpt, bucket) =>
                    val dialectSuffix = dialectOpt.map(d => s" ($d)").getOrElse("")
                    val toplevels = // pprint.PPrinter.BlackWhite.apply(
                      bucket.toplevels.trieMap.iterator.toVector.sortBy(_._1).map {
                        case (k, v) =>
                          val v0 = v.toVector
                            .map {
                              case (p, opt) =>
                                p.uri + opt.map {
                                  case Left(p)    => p.toString
                                  case Right(mod) => mod.targetId
                                }.fold("")(v => s" ($v)")
                            }
                          k + System.lineSeparator() +
                            v0.map("  " + _ + System.lineSeparator()).mkString
                      }.mkString
                    val definitions = bucket.definitions.trieMap
                      .map {
                        case (sym, a) =>
                          s"$sym: $a" + System.lineSeparator()
                      }
                      .mkString
                    s"  Toplevels$dialectSuffix" + System.lineSeparator() + System.lineSeparator() +
                      toplevels + System.lineSeparator() + System.lineSeparator() +
                      s"  Definitions$dialectSuffix" + System.lineSeparator() + System.lineSeparator() +
                      definitions + System.lineSeparator() + System.lineSeparator()
                }
                .mkString
              val shortTargetId = BspUtil.targetShortId(server.bspData, targetId)
                .takeWhile(_ != '?') // meh
              (shortTargetId, content)
            }
          val resp = contentOrError match {
            case Left(err) =>
              DebugSymbolIndexResp("", "", err)
            case Right((shortTargetId, content)) =>
              DebugSymbolIndexResp(shortTargetId, content, "")
          }
          CompletableFuture.completedFuture(writeToGson(resp))
        }
      },
      CommandHandler.of("plasmon/debugFullTree") { (params, logger) =>
        params.asFileUri("plasmon/debugFullTree") { file =>
          val buildTargetOpt = server.bspData.inverseSources(file)
          val contentOrError =
            buildTargetOpt.toRight(s"No build target found for $file").flatMap { targetId =>
              server.presentationCompilers
                .compile(
                  new l.TextDocumentIdentifier(file.toNIO.toUri.toASCIIString),
                  targetId,
                  EmptyCancelToken
                )
                .map((targetId, _))
                .toRight(s"No presentation compiler found for target ${targetId.getUri}")
            }
          contentOrError match {
            case Left(err) =>
              val resp0 = DebugFullTreeResp("", "", "", err)
              CompletableFuture.completedFuture(writeToGson(resp0))
            case Right((targetId, future)) =>
              future
                .map { res =>
                  val resp = DebugFullTreeResp(
                    targetId.getUri,
                    res.fullTree(),
                    res.diagnostics().asScala.mkString(System.lineSeparator()),
                    ""
                  )
                  writeToGson(resp): Object
                }(server.pools.dummyEc)
                .asJava
          }
        }
      },
      CommandHandler.of("plasmon/debugBspData") { (params, logger) =>
        params.asFileUri("plasmon/debugBspData") { file =>
          val buildTargetOpt = server.bspData.inverseSources(file)
          val contentOrError =
            buildTargetOpt.toRight(s"No build target found for $file").map { targetId =>
              val res = server.bspData.allWritableData
                .filter(_.buildTargetInfo.contains(targetId))
                .map { data =>
                  val out = new StringBuilder
                  val nl  = System.lineSeparator()
                  out ++= s"Target ${targetId.getUri}" + nl + nl
                  out ++= data.buildTargetInfo(targetId).toString + nl + nl
                  for (javaTarget <- data.javaTargetInfo.get(targetId))
                    out ++= javaTarget.javac.toString + nl + nl
                  for (scalaTarget <- data.scalaTargetInfo.get(targetId)) {
                    out ++= scalaTarget.scalac.toString + nl + nl
                    out ++= scalaTarget.scalaInfo.toString + nl + nl
                    for (autoImports <- scalaTarget.autoImports)
                      out ++= "Auto-imports: " + autoImports.toString + nl + nl
                    for (sbtVersion <- scalaTarget.sbtVersion)
                      out ++= "sbt version: " + sbtVersion + nl + nl
                  }
                  for (inverseDeps <- data.inverseDependencies.get(targetId))
                    out ++= inverseDeps.toString + nl + nl
                  for (elem <- data.buildTargetSources.get(targetId))
                    out ++= "Sources: " + elem.toString + nl + nl
                  for (cp <- data.buildTargetClasspath.get(targetId))
                    out ++= "Class path: " + cp.toString + nl + nl
                  for (depMods <- data.buildTargetDependencyModules.get(targetId))
                    out ++= "Dependency modules: " + depMods.toString + nl + nl
                  for (workspace <- data.targetToWorkspace.get(targetId))
                    out ++= "Workspace: " + workspace.toString + nl + nl
                  out.result()
                }
                .mkString
              (targetId, res)
            }
          val resp = contentOrError match {
            case Left(err) =>
              DebugBspDataResp("", "", err)
            case Right((targetId, out)) =>
              DebugBspDataResp(targetId.getUri, out, "")
          }
          CompletableFuture.completedFuture(writeToGson(resp))
        }
      },
      CommandHandler.of("plasmon/debugSemanticdbLookup") { (params, logger) =>
        params.asFileUri("plasmon/debugSemanticdbLookup") { file =>
          val buildTargetOpt = server.bspData.inverseSources(file)
          val contentOrError =
            buildTargetOpt.toRight(s"No build target found for $file").map { targetId =>
              val res = server.fileSystemSemanticdbs.textDocument0(file, targetId)
              val text = res match {
                case Left(err) =>
                  "Error: " + err
                case Right(value) =>
                  value.toString
              }
              (targetId, text)
            }
          val resp = contentOrError match {
            case Left(err) =>
              DebugSemanticdbLookupResp("", "", err)
            case Right((targetId, out)) =>
              DebugSemanticdbLookupResp(targetId.getUri, out, "")
          }
          CompletableFuture.completedFuture(writeToGson(resp))
        }
      },
      CommandHandler.of("plasmon/debugPresentationCompiler") { (params, logger) =>
        params.as[Boolean]("plasmon/debugPresentationCompiler") { enable =>
          val changed = server.presentationCompilers.setDebug(enable)
          val resp = DebugPcResp(
            enabled = enable,
            changed = changed,
            PresentationCompilers.logDest(server.workspace()).toString
          )
          CompletableFuture.completedFuture(writeToGson(resp))
        }
      }
    )

  def commandHandlers(server: Server, indexer: Indexer, pools: ServerCommandThreadPools) =
    buildToolsCommands(server, indexer, pools) ++
      listModulesCommands(server, indexer, pools) ++
      reindexCommands(server, indexer, pools) ++
      interactiveCompilerCommands(server) ++
      debugCommands(server)

  def handlers(server: Server, indexer: Indexer, pools: ServerCommandThreadPools): Handlers =
    Handlers(Nil, Nil, commandHandlers(server, indexer, pools))
}
