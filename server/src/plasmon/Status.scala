package plasmon

import ch.epfl.scala.{bsp4j => b}
import plasmon.bsp.{BuildServerInfo, BuildServerProcess}
import plasmon.languageclient._
import plasmon.servercommand.BspUtil

import java.time.Instant
import java.util.concurrent.ConcurrentHashMap
import plasmon.semdb.TextDocumentLookup
import scala.meta.internal.mtags.SourcePath
import scala.meta.internal.pc.HasCompilerAccess
import java.util.concurrent.ScheduledExecutorService
import java.util.concurrent.TimeUnit
import java.util.concurrent.CompletableFuture
import java.util.concurrent.ScheduledFuture
import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.CompletionException
import org.eclipse.lsp4j.jsonrpc.ResponseErrorException
import org.eclipse.lsp4j.jsonrpc.messages.ResponseErrorCode
import plasmon.bsp.BspConnection

import plasmon.pc.PresentationCompilers

import plasmon.PlasmonEnrichments._
import scala.jdk.CollectionConverters._
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker

class Status(
  server: Server,
  healthCheckScheduler: ScheduledExecutorService
) {

  import Status._

  private val semdbStatusCache =
    new ConcurrentHashMap[(b.BuildTargetIdentifier, os.Path), SemdbStatusDetails]

  private val buildToolStatusCache = new ConcurrentHashMap[BuildServerInfo, BuildToolStatus]

  private def updateBuildToolStatuses(): Unit = {
    // val connections = server.bspServers.list.flatMap(_._2)
    //
    // lazy val allInfos = connections.map(_.info).toSet
    // for (key <- buildToolStatusCache.keys().asScala.toVector if !allInfos.contains(key))
    //   buildToolStatusCache.remove(key)
    //
    // for (conn <- connections) {
    //   val status = Option(buildToolStatusCache.get(conn.info)).getOrElse {
    //     val candidate   = BuildToolStatus()
    //     val existingOpt = Option(buildToolStatusCache.putIfAbsent(conn.info, candidate))
    //     existingOpt.getOrElse(candidate)
    //   }
    //   val updatedStatusOpt = status.check(conn, healthCheckScheduler, () => server.refreshStatus())
    //   for (updatedStatus <- updatedStatusOpt)
    //     buildToolStatusCache.put(conn.info, updatedStatus)
    // }
  }

  private def showLogCommand(logId: String) =
    PlasmonLanguageClient.Command(
      title = "Show log",
      command = "plasmon.show-log",
      tooltip = "Show log",
      arguments = List(logId).asJava
    )

  private def showDocCommand(
    docName: String,
    docContent: String,
    docLanguage: Option[String]
  ) =
    PlasmonLanguageClient.Command(
      title = "Show document",
      command = "plasmon.show-doc",
      tooltip = "Show doc",
      arguments = (List(docName, docContent) ++ docLanguage.toList).asJava
    )

  private def indexerUpdate(
    text: String,
    detail: String,
    busy: Boolean,
    loggerId: Option[String],
    severity: Int = 0
  ) =
    PlasmonLanguageClient.StatusUpdate(
      id = "plasmon.indexer",
      text = text,
      severity = severity,
      busy = busy,
      detail = detail,
      command = loggerId.map(showLogCommand(_)).orNull
    )

  private def buildToolUpdate(
    text: String,
    detail: String,
    severity: Int,
    command: PlasmonLanguageClient.Command = null,
    busy: Boolean = false
  ) =
    PlasmonLanguageClient.StatusUpdate(
      id = "plasmon.build-tool",
      text = text,
      severity = severity,
      busy = busy,
      detail = detail,
      command = command
    )

  private def compilerUpdate(
    text: String,
    severity: Int = 0,
    busy: Boolean = false,
    buildTarget: Option[b.BuildTargetIdentifier] = None,
    commandOpt: Option[PlasmonLanguageClient.Command] = None
  ) =
    PlasmonLanguageClient.StatusUpdate(
      id = "plasmon.compiler",
      text = text,
      severity = severity,
      busy = busy,
      detail =
        buildTarget.map(id => "Module " + BspUtil.targetShortId(server.bspData, id)).orNull,
      command = commandOpt.getOrElse {
        buildTarget match {
          case Some(targetId) =>
            val loggerIdOpt = server.bspData.buildServerOf(targetId).flatMap { buildServer =>
              server.bspServers.list.iterator
                .flatMap(_._2.iterator)
                .find(_.conn == buildServer)
                .flatMap(_.client.logger)
                .map(_.channel.id)
            }
            loggerIdOpt
              .map(showLogCommand(_))
              .orNull
          case None =>
            PlasmonLanguageClient.Command(
              title = "Look for build tool or modules",
              command = "plasmon.load-build-tool-or-module",
              tooltip = "Look for build tools or modules"
            )
        }
      }
    )

  private def interactiveUpdate(
    scalaOrJavaVersionOpt: Option[Either[String, String]],
    text: String,
    busy: Boolean,
    severity: Int = 0,
    compilerKeyOpt: Option[PresentationCompilers.PresentationCompilerKey] = None
  ) =
    PlasmonLanguageClient.StatusUpdate(
      id = "plasmon.interactive",
      text = text,
      severity = severity,
      busy = busy,
      detail = scalaOrJavaVersionOpt match {
        case None                => "Scala Interactive"
        case Some(Left(javaVer)) => s"Javac $javaVer"
        case Some(Right(sv))     => s"Scala $sv Interactive"
      },
      command = compilerKeyOpt
        .flatMap { compilerKey =>
          (compilerKey, scalaOrJavaVersionOpt) match {
            case (
                  PresentationCompilers.PresentationCompilerKey.ScalaBuildTarget(id),
                  Some(Right(sv))
                ) =>
              Some(s"plasmon-interactive-$sv-${BspUtil.targetShortId(server.bspData, id)}")
            case (
                  PresentationCompilers.PresentationCompilerKey.ScalaBuildTarget(id),
                  Some(Left(_))
                ) =>
              Some(s"plasmon-java-presentation-compiler-$id")
            case (
                  PresentationCompilers.PresentationCompilerKey.JavaBuildTarget(_),
                  Some(Right(_))
                ) =>
              ???
            case (
                  PresentationCompilers.PresentationCompilerKey.JavaBuildTarget(_),
                  Some(Left(_))
                ) =>
              ???
            case (_, None) =>
              None
          }
        }
        .map { loggerId =>
          PlasmonLanguageClient.Command(
            title = "Interactive",
            command = "plasmon.show-log",
            tooltip = "Show interactive compiler log",
            arguments = List(loggerId).asJava
          )
        }
        .orNull
    )

  private def asSummaryUpdate(update: PlasmonLanguageClient.StatusUpdate)
    : PlasmonLanguageClient.StatusUpdate =
    update.copy(
      id = "plasmon.summary",
      detail = null
    )

  private def defaultBuildToolUpdateOpt() = {
    val (states, loggerOpt) = server.bspServers.state()
    states.headOption.map { state =>
      buildToolUpdate(
        state,
        "",
        severity = 1,
        busy = true,
        command = loggerOpt.map(logger => showLogCommand(logger.channel.id)).orNull
      )
    }
  }

  def semdbStatus(targetId: b.BuildTargetIdentifier, source: os.Path): SemdbStatus =
    Option(semdbStatusCache.get((targetId, source)))
      .filter(!_.shouldUpdate(source))
      .map(_.status(source))
      .getOrElse {
        val mtime = Instant.ofEpochMilli(os.mtime(source))
        val lookup = server.fileSystemSemanticdbs
          .textDocument0(source, targetId)
        lookup match {
          case Right(s: TextDocumentLookup.Success) =>
            val details = SemdbStatusDetails(
              mtime,
              s.path,
              Instant.ofEpochMilli(os.mtime(s.path)),
              s.document.text
            )
            semdbStatusCache.put((targetId, source), details)
            details.status(source)
          case _ =>
            SemdbStatus.NotFound
        }
      }

  def plasmonFileStatus(): Option[(os.Path, Seq[PlasmonLanguageClient.StatusUpdate])] =
    server.editorState.focusedDocument
      .map(path => (path, plasmonFileStatus(path)))

  def plasmonFileStatus(path: os.Path): Seq[PlasmonLanguageClient.StatusUpdate] = {
    updateBuildToolStatuses()

    val (indexerUpdate0, indexerSummaryUpdateOpt) = {
      val (state, loggerOpt) = server.currentIndexerState()
      val update = indexerUpdate(
        state.headOption.getOrElse(""),
        "",
        busy = state.nonEmpty,
        loggerId = loggerOpt.map(_.channel.id)
      )
      val summaryUpdateOpt =
        if (state.isEmpty) None
        else
          Some {
            indexerUpdate(
              state.lastOption.getOrElse(""),
              "",
              busy = true,
              severity = 1,
              loggerId = loggerOpt.map(_.channel.id)
            )
          }
      (update, summaryUpdateOpt)
    }

    val sharedUpdates = Seq(
      indexerUpdate0
    )

    val fileUpdates =
      if (os.isFile(path) && path.isScalaOrJava) {
        val buildTargetRes = server.bspData.inverseSources0(path)

        def interactiveUpdate0(buildTargetOpt: Option[b.BuildTargetIdentifier]) = {

          val isJava = path.isJava

          if (isJava)
            interactiveUpdate(
              None,
              "",
              busy = false
            )
          else {

            val compilerKeyOpt =
              buildTargetOpt.map(PresentationCompilers.PresentationCompilerKey.ScalaBuildTarget(_))

            lazy val pathUri = path.toNIO.toUri.toASCIIString

            val compilerOpt =
              compilerKeyOpt.flatMap(key => Option(server.presentationCompilers.jcache.get(key)))
            val scalaVersionOpt: Option[String] =
              compilerOpt.map(c => c: PresentationCompilers.MtagsPresentationCompiler).flatMap {
                case l: PresentationCompilers.ScalaLazyCompiler => Some(l.scalaTarget.scalaVersion)
                case _                                          => None
              }
            val exceptionMessageOpt = compilerOpt
              .map(c => c: PresentationCompilers.MtagsPresentationCompiler)
              .flatMap {
                case l: PresentationCompilers.ScalaLazyCompiler => l.compilerOpt
                case _                                          => None
              }
              .collect {
                case h: HasCompilerAccess =>
                  h.latestException()
              }
              .flatten
              .filter {
                case (Some(uri), _) =>
                  uri == pathUri
                case (None, _) =>
                  false
              }
              .map(_ => "Errored")
            val onGoingOpt = compilerOpt
              .flatMap(pc =>
                Option(server.presentationCompilers.interactiveCompilersStatuses.get(pc))
              )
              .collect {
                case (name, uri) if uri == pathUri =>
                  name
              }
              .filter(_.nonEmpty)
              .map(_.capitalize)
            val onGoingCompletionOpt =
              compilerKeyOpt.flatMap(key =>
                Option(server.presentationCompilers.jCompletionCache.get(key))
              )
                .flatMap(pc =>
                  Option(server.presentationCompilers.interactiveCompilersStatuses.get(pc))
                )
                .collect {
                  case (name, uri) if uri == pathUri =>
                    name
                }
                .filter(_.nonEmpty)
                .map(_.capitalize)

            val onGoing = onGoingOpt.orElse(exceptionMessageOpt).toSeq ++ onGoingCompletionOpt.toSeq

            interactiveUpdate(
              scalaVersionOpt.map(Right(_)),
              if (onGoing.isEmpty)
                if (scalaVersionOpt.isEmpty)
                  "Not instantiated"
                else
                  "Idle"
              else
                onGoing.mkString(" | "),
              busy = onGoingOpt.nonEmpty || onGoingCompletionOpt.nonEmpty,
              compilerKeyOpt = compilerKeyOpt,
              severity = if (exceptionMessageOpt.isEmpty) 0 else 2
            )
          }
        }

        buildTargetRes match {
          case Right(targets) =>
            val buildTargetOpt = targets.headOption // FIXME order

            val serverOpt = buildTargetOpt.map { buildTarget =>
              server.bspData.buildServerOf(buildTarget) match {
                case Some(server0) =>
                  server.bspServers.list
                    .flatMap(_._2)
                    .find(_.conn == server0)
                    .toRight("unknown")
                case None => Left("not found")
              }
            }

            val (buildToolUpdate0, buildToolUpdatePrecedence) = serverOpt match {
              case Some(Right(bspDetails)) =>
                val pidDetails = bspDetails.proc match {
                  case p: BuildServerProcess.Process =>
                    val pid = p.proc.wrapped.pid()
                    Seq(s"PID $pid")
                  case _ =>
                    Nil
                }
                val bspDetails0 =
                  if (bspDetails.info.`type` == "BSP") Nil
                  else Seq(bspDetails.info.`type`)
                val connTypeDetails =
                  bspDetails0 ++
                    Seq(s"BSP ${bspDetails.params.getBspVersion}") ++
                    pidDetails
                val (title, tooltip) =
                  if (bspDetails.proc.managedProcess) ("Restart", "Restart build tool")
                  else ("Reconnect", "Reconnect to build tool")
                val (statusPart, severity) = Option(buildToolStatusCache.get(bspDetails.info))
                  .flatMap(_.status.get().message)
                  .getOrElse(("", 0))
                val statusPart0 = if (statusPart.isEmpty()) "" else s" ($statusPart)"
                val update = buildToolUpdate(
                  s"${bspDetails.params.getDisplayName} ${bspDetails.params.getVersion}$statusPart0",
                  connTypeDetails.mkString(", "),
                  severity = severity,
                  command = PlasmonLanguageClient.Command(
                    title = title,
                    command = "plasmon.build-tool-restart",
                    tooltip = tooltip,
                    arguments = List(bspDetails.info.workspace.toString, bspDetails.name).asJava
                  )
                )
                (update, false)

              case Some(Left(error)) =>
                val update = buildToolUpdate("Build tool error", error, severity = 2)
                (update, true)

              case None =>
                val update = defaultBuildToolUpdateOpt().getOrElse {
                  val hasBuildTool = server.bspData.allTargetData.exists { data =>
                    data.workspaceBuildTargetsRespOpt.exists { resp =>
                      resp.baseDirectories.exists(path.startsWith)
                    }
                  }
                  buildToolUpdate(
                    if (hasBuildTool) "Not loaded" else "No build tool",
                    "",
                    severity = 2,
                    command =
                      PlasmonLanguageClient.Command(
                        title = "Look for build tools or modules",
                        command = "plasmon.load-build-tool-or-module",
                        tooltip = "Look for build tools or modules"
                      )
                  )
                }
                (update, true)
            }

            // val (moduleUpdate0, moduleUpdatePrecedence) = buildTargetOpt match {
            //   case Some(buildTarget) =>
            //     val update = moduleUpdate(
            //       BspUtil.targetShortId(server.bspData, buildTarget),
            //       command = PlasmonLanguageClient.Command(
            //         title = "Re-index",
            //         command = Server.ClientCommandNames.index,
            //         tooltip = "Re-index loaded modules"
            //       )
            //     )
            //     (update, false)
            //   case None =>
            //     val update = moduleUpdate(
            //       "Not found",
            //       severity = 2,
            //       command =
            //         PlasmonLanguageClient.Command(
            //           title = "Look for build tool or modules",
            //           command = "plasmon.load-build-tool-or-module",
            //           tooltip = "Look for build tools or modules"
            //         )
            //     )
            //     (update, true)
            // }

            val compilerUpdate0 = buildTargetOpt match {
              case Some(buildTarget) =>
                server.compilations.latestCompilations.get(buildTarget) match {
                  case Some((_, Some(_))) =>
                    def progressPartOf(targetId: b.BuildTargetIdentifier): Option[String] =
                      serverOpt.flatMap(_.toOption).map(_.client).flatMap { buildClient =>
                        buildClient.progressFor(targetId.getUri).map {
                          case (progress, total) =>
                            val pct = math.round((100 * progress).toDouble / total)
                            s" ($pct%)"
                        }
                      }
                    val progressPartOpt = progressPartOf(buildTarget)
                    def dependencyMessageOpt = server.bspData
                      .buildTargetTransitiveDependencies(buildTarget)
                      .iterator
                      .flatMap { depTargetId =>
                        progressPartOf(depTargetId)
                          .iterator
                          .map { progressPart =>
                            val id = BspUtil.targetShortId(server.bspData, depTargetId)
                            val id0 =
                              if (id.length() > 20) id.take(20) + "…"
                              else id
                            s"Compiling $id0$progressPart"
                          }
                      }
                      .find(_ => true)
                    val message = progressPartOpt match {
                      case Some(progressPart) => "Compiling…" + progressPart
                      case None               => dependencyMessageOpt.getOrElse("Compiling…")
                    }
                    compilerUpdate(
                      message,
                      severity = 1,
                      busy = true,
                      buildTarget = Some(buildTarget)
                    )
                  case Some((Some(pastCompilation), None)) =>
                    val isStale = os.mtime(path) > pastCompilation.startTime.toInstant.toEpochMilli
                    val infoSuffix = if (isStale) " (stale)" else ""
                    pastCompilation.result.getStatusCode match {
                      case b.StatusCode.OK =>
                        val semdbStatus0 = semdbStatus(buildTarget, path) match {
                          case SemdbStatus.Stale if isStale => SemdbStatus.UpToDate
                          case other                        => other
                        }
                        semdbStatus0 match {
                          case SemdbStatus.UpToDate =>
                            compilerUpdate(
                              "Compiled" + infoSuffix,
                              buildTarget = Some(buildTarget)
                            )
                          case SemdbStatus.Stale =>
                            compilerUpdate(
                              "Stale semantic info",
                              buildTarget = Some(buildTarget),
                              severity = 1
                            )
                          case SemdbStatus.NotFound =>
                            compilerUpdate(
                              "Missing semantic info",
                              buildTarget = Some(buildTarget),
                              severity = 1,
                              commandOpt = Some(
                                PlasmonLanguageClient.Command(
                                  title = "Get semanticdb lookup result",
                                  command = "plasmon.dump-semanticdb-details",
                                  tooltip = "Get semanticdb lookup result"
                                )
                              )
                            )
                        }
                      case b.StatusCode.ERROR =>
                        compilerUpdate(
                          "Compilation failed" + infoSuffix,
                          severity = 2,
                          buildTarget = Some(buildTarget)
                        )
                      case b.StatusCode.CANCELLED =>
                        compilerUpdate(
                          "Compilation cancelled" + infoSuffix,
                          severity = 2,
                          buildTarget = Some(buildTarget)
                        )
                    }
                  case _ =>
                    compilerUpdate(
                      "Not compiled",
                      severity = 2,
                      buildTarget = Some(buildTarget)
                    )
                }
              case None =>
                compilerUpdate(
                  "Unknown file",
                  severity = 2
                )
            }

            val summaryUpdate =
              indexerSummaryUpdateOpt.getOrElse {
                if (buildToolUpdatePrecedence)
                  buildToolUpdate0
                else
                  compilerUpdate0
              }

            Seq(
              buildToolUpdate0,
              compilerUpdate0,
              interactiveUpdate0(buildTargetOpt),
              asSummaryUpdate(summaryUpdate)
            )

          case Left(inferredTargets) =>
            val buildTargetOpt = inferredTargets.headOption // FIXME order

            val interactiveUpdate = interactiveUpdate0(buildTargetOpt)

            val summaryUpdate = indexerSummaryUpdateOpt.getOrElse(interactiveUpdate)

            Seq(interactiveUpdate, asSummaryUpdate(summaryUpdate))
        }
      }
      else {
        val buildToolUpdate0 = defaultBuildToolUpdateOpt().getOrElse {
          buildToolUpdate("File not found", "", severity = 2)
        }
        val compilerUpdate0 = compilerUpdate("File not found", severity = 2)
        val interactiveUpdate0 = interactiveUpdate(
          None,
          "File not found",
          busy = false,
          severity = 2
        )
        val summaryUpdate = indexerSummaryUpdateOpt.getOrElse(buildToolUpdate0)
        Seq(buildToolUpdate0, compilerUpdate0, interactiveUpdate0, asSummaryUpdate(summaryUpdate))
      }

    sharedUpdates ++ fileUpdates
  }

  def asJson: Status.AsJson =
    Status.AsJson()
}

object Status {

  private final case class SemdbStatusDetails(
    lastModified: Instant,
    semdbPath: os.Path,
    semdbLastModified: Instant,
    text: String
  ) {
    // race conditions likely around these things
    def shouldUpdate(path: os.Path): Boolean =
      !os.exists(path) || os.mtime(path) != lastModified.toEpochMilli ||
      !os.exists(semdbPath) || os.mtime(semdbPath) != semdbLastModified.toEpochMilli ||
      semdbLastModified.compareTo(lastModified) < 0 && os.read(path) != text
    def status(path: os.Path): SemdbStatus =
      if (os.exists(path))
        if (shouldUpdate(path)) SemdbStatus.Stale
        else SemdbStatus.UpToDate
      else
        SemdbStatus.NotFound
  }
  sealed abstract class SemdbStatus extends Product with Serializable
  object SemdbStatus {
    case object UpToDate extends SemdbStatus
    case object Stale    extends SemdbStatus
    case object NotFound extends SemdbStatus
  }

  private sealed abstract class BuildServerState(val message: Option[(String, Int)]) extends Product
      with Serializable

  private object BuildServerState {
    case object Running extends BuildServerState(None)
    case object Errored extends BuildServerState(Some(("connection error", 2)))
    case object Unknown extends BuildServerState(Some(("unknown state", 1)))
  }

  private final case class BuildToolStatus(
    time: Instant = Instant.now(),
    errorOpt: Option[Throwable] = None,
    onGoingCheck: Option[CompletableFuture[?]] = None,
    onGoingScheduleCheck: Option[ScheduledFuture[?]] = None,
    status: AtomicReference[BuildServerState] =
      new AtomicReference[BuildServerState](BuildServerState.Unknown)
  ) {
    def check(
      conn: BspConnection,
      healthCheckScheduler: ScheduledExecutorService,
      onNewStatus: () => Unit
    ): Option[BuildToolStatus] =
      if (onGoingScheduleCheck.forall(f => f.isCancelled() || f.isDone())) {
        if (!onGoingScheduleCheck.forall(f => f.isCancelled() || f.isDone()))
          for (f <- onGoingCheck) {
            val cancelled = f.cancel(true) || f.isCancelled()
            if (!cancelled)
              scribe.warn(s"Could not cancel build tool on going health request for ${conn.name}")
          }
        val future = conn.remoteEndpoint.request("plasmon/doesnt-exist", null)
        val futureCheck = healthCheckScheduler.schedule(
          new Runnable {
            def run(): Unit = {
              val updatedStatus =
                if (future.isCancelled() || future.isDone())
                  BuildServerState.Unknown
                else if (future.isCompletedExceptionally())
                  try {
                    future.join()
                    BuildServerState.Unknown
                  }
                  catch {
                    case ex: CompletionException =>
                      ex.getCause match {
                        case ex: ResponseErrorException
                            if ex.getResponseError.getCode == ResponseErrorCode.MethodNotFound.getValue =>
                          BuildServerState.Running
                        case ex0: Throwable =>
                          scribe.info("Got BSP health check exception", ex0)
                          BuildServerState.Unknown
                      }
                  }
                else {
                  future.cancel(true)
                  BuildServerState.Unknown
                }
              status.set(updatedStatus)
              onNewStatus()
            }
          },
          5L,
          TimeUnit.SECONDS
        )
        Some(
          copy(
            onGoingCheck = Some(future),
            onGoingScheduleCheck = Some(futureCheck)
          )
        )
      }
      else
        None
  }

  final case class AsJson()

  given JsonValueCodec[AsJson] =
    JsonCodecMaker.make
}
