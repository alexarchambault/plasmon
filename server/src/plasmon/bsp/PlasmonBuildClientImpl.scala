package plasmon.bsp

import ch.epfl.scala.{bsp4j => b}
import com.github.plokhotnyuk.jsoniter_scala.core._
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import com.google.gson.Gson
import org.eclipse.lsp4j.jsonrpc.services.JsonNotification
import org.eclipse.{lsp4j => l}

import scala.jdk.CollectionConverters._

import java.util.concurrent.ConcurrentHashMap

import plasmon.Logger
import java.net.URI
import plasmon.languageclient.PlasmonLanguageClient
import java.util.concurrent.atomic.AtomicInteger
import plasmon.ide.Buffers
import plasmon.ide.Trees
import scala.meta.internal.mtags.GlobalSymbolIndex
import plasmon.PlasmonEnrichments._

class PlasmonBuildClientImpl(
  languageClient: PlasmonLanguageClient,
  buffers: Buffers,
  trees: Trees,
  workspace: os.Path,
  onBuildTargetDidChangeFunc: b.DidChangeBuildTarget => Unit
) extends PlasmonBuildClient with AutoCloseable {

  import PlasmonBuildClientImpl.*

  private val diagnostics = new Diagnostics(buffers, languageClient, workspace, trees)

  private val requestCount = new AtomicInteger
  private var loggerOpt    = Option.empty[Logger]

  private var compilationClients0 = Map.empty[String, PlasmonBuildClient.CompilationClient]
  def addClient(originId: String, client: PlasmonBuildClient.CompilationClient): Unit = {
    compilationClients0 = compilationClients0 + (originId -> client)
  }
  def removeClient(originId: String): Unit = {
    compilationClients0 = compilationClients0 - originId
  }

  private def onBuildMessage(params: l.MessageParams): Unit =
    for (logger <- loggerOpt) {
      val prefix = params.getType match {
        case l.MessageType.Error   => "[error] "
        case l.MessageType.Warning => "[warn] "
        case l.MessageType.Info    => "[info] "
        case l.MessageType.Log     => ""
      }
      for (line <- params.getMessage.linesIterator)
        logger.log(prefix + line)
    }

  @JsonNotification("build/showMessage")
  def onBuildShowMessage(params: l.MessageParams): Unit = {
    onBuildMessage(params)
    languageClient.showMessage(params)
  }

  @JsonNotification("build/logMessage")
  def onBuildLogMessage(params: l.MessageParams): Unit = {
    onBuildMessage(params)

    if (params.getMessage.nonEmpty)
      params.getType match {
        case l.MessageType.Error =>
          scribe.error(params.getMessage)
        case l.MessageType.Warning =>
          scribe.warn(params.getMessage)
        case l.MessageType.Info =>
          scribe.info(params.getMessage)
        case l.MessageType.Log =>
          scribe.info(params.getMessage)
      }
  }

  @JsonNotification("build/publishDiagnostics")
  def onBuildPublishDiagnostics(params: b.PublishDiagnosticsParams): Unit = {
    for (client <- compilationClients0.get(params.getOriginId))
      client.onBuildPublishDiagnostics(params)

    for (logger <- loggerOpt) {
      val uriStr = params.getTextDocument.getUri
      val uri    = new URI(uriStr)
      val path =
        if (uri.getScheme == "file") Right(uriStr.osPathFromUri)
        else Left(uriStr)
      val logger0 =
        logger.addPrefix(s"[${requestCount.incrementAndGet()}-build/publishDiagnostics] ")
      if (params.getReset)
        logger0.log(s"Reset ${path.map(_.toString).merge} diagnostics")
      if (params.getDiagnostics.size() > 0)
        logger0.log(
          s"Got ${params.getDiagnostics.size()} diagnostic(s) for ${path.map(_.toString).merge}"
        )
    }

    diagnostics.onBuildPublishDiagnostics(params)
  }

  @JsonNotification("buildTarget/didChange")
  def onBuildTargetDidChange(params: b.DidChangeBuildTarget): Unit =
    onBuildTargetDidChangeFunc(params)

  private var onProgress0 = Seq.empty[String => Unit]

  def onProgress(f: String => Unit): Unit =
    onProgress0 = onProgress0 :+ f

  private val tasksProgress          = new ConcurrentHashMap[String, TaskDetails]
  private val tasksProgressIdFromUri = new ConcurrentHashMap[String, String]

  def buildTaskStart(params: b.TaskStartParams): Unit = {

    for (logger <- loggerOpt) {
      val msg = s"Task ${params.getTaskId.getId} starting" +
        Option(params.getMessage).fold("")(msg => s": $msg")
      logger.log(msg)
      for (data <- Option(params.getData))
        logger.log(s"Data${Option(params.getDataKind).fold("")(kind => s" of kind $kind")}: $data")
    }

    if (params.getDataKind == "compile-task") {
      val data = readFromGson[CompileTaskData](params.getData)
      val uri  = data.target.uri
      val details = TaskDetails(
        params.getTaskId.getId,
        uri,
        None
      )
      val success = tasksProgress.putIfAbsent(details.id, details) == null
      if (success)
        tasksProgressIdFromUri.put(details.uri, details.id)
      onProgress0.foreach(_(details.uri))
    }
    else
      scribe.info("build/taskStart: " + pprint.apply(params))
  }

  def buildTaskProgress(params: b.TaskProgressParams): Unit = {

    for (logger <- loggerOpt) {
      val msg = s"Task ${params.getTaskId.getId} progress" +
        Option(params.getProgress).fold("")(p => s" $p") +
        Option(params.getTotal).fold("")(t => s" / $t") +
        Option(params.getMessage).fold("")(msg => s" ($msg)")
      logger.log(msg)
      for (data <- Option(params.getData))
        logger.log(s"Data${Option(params.getDataKind).fold("")(kind => s" of kind $kind")}: $data")
    }

    Option(tasksProgress.get(params.getTaskId.getId)) match {
      case Some(details) =>
        val updatedDetails = details.copy(progress = Some((params.getProgress, params.getTotal)))
        val replaced       = tasksProgress.replace(details.id, details, updatedDetails)
        if (!replaced)
          scribe.warn(s"Could not keep track of progress for ${details.uri} via $params")
        onProgress0.foreach(_(details.uri))
      case None =>
        scribe.info("build/taskProgress: " + pprint.apply(params))
    }
  }

  def buildTaskFinish(params: b.TaskFinishParams): Unit = {

    for (logger <- loggerOpt) {
      val msg = s"Task ${params.getTaskId.getId} done" + Option(params.getMessage).fold("")(msg =>
        s": $msg"
      )
      logger.log(msg)
      for (data <- Option(params.getData))
        logger.log(s"Data${Option(params.getDataKind).fold("")(kind => s" of kind $kind")}: $data")
    }

    Option(tasksProgress.get(params.getTaskId.getId)) match {
      case Some(details) =>
        val success = tasksProgress.remove(details.id) != null
        if (success)
          tasksProgressIdFromUri.remove(details.uri)
        onProgress0.foreach(_(details.uri))
      case None =>
        scribe.info("build/taskFinish: " + pprint.apply(params))
    }
  }

  def progressFor(uri: String): Option[(Long, Long)] = {
    val idOpt      = Option(tasksProgressIdFromUri.get(uri))
    val detailsOpt = idOpt.flatMap(id => Option(tasksProgress.get(id)))
    if (idOpt.isEmpty)
      scribe.info(
        s"No progress for $uri (available for: ${tasksProgressIdFromUri.keys().asScala.toVector.sorted})"
      )
    else if (detailsOpt.isEmpty)
      scribe.info(
        s"No progress for $uri (${idOpt.get}) (available for ids: ${tasksProgress.keys().asScala.toVector.sorted})"
      )
    detailsOpt.flatMap(_.progress)
  }

  def logger: Option[Logger] =
    loggerOpt
  def setLogger(logger: Logger): Unit = {
    loggerOpt = Some(logger)
  }

  def diagDidChange(module: GlobalSymbolIndex.Module, path: os.Path): Unit =
    diagnostics.diagDidChange(module, path)
  def onClose(module: GlobalSymbolIndex.Module, path: os.Path): Unit =
    diagnostics.onClose(module, path)
  def didDelete(path: os.Path): Unit =
    diagnostics.didDelete(path)
  def diagnosticsFor(path: os.Path): Seq[l.Diagnostic] =
    diagnostics.diagnostics.get(path).map(_.asScala.toSeq).getOrElse(Nil)
  def toFreshDiagnostic(
    module: GlobalSymbolIndex.Module,
    path: os.Path,
    d: l.Diagnostic
  ): Option[l.Diagnostic] =
    diagnostics.toFreshDiagnostic(module, path, d)
  def onSyntaxError(
    module: GlobalSymbolIndex.Module,
    path: os.Path,
    diags: List[l.Diagnostic]
  ): Unit =
    diagnostics.onSyntaxError(module, path, diags)

  def close(): Unit = {
    diagnostics.close()
  }
}

object PlasmonBuildClientImpl {
  private final case class CompileTaskData(
    target: CompileTaskDataTarget
  )
  private final case class CompileTaskDataTarget(
    uri: String
  )
  private implicit lazy val compileTaskDataCodec: JsonValueCodec[CompileTaskData] =
    JsonCodecMaker.make

  private final case class TaskDetails(
    id: String,
    uri: String,
    progress: Option[(Long, Long)]
  )

  // quite ineffective, but does the job
  private def readFromGson[T: JsonValueCodec](obj: Object, gson: Gson = new Gson): T =
    readFromString[T](gson.toJson(obj))
}
