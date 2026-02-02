package plasmon.languageclient

import org.eclipse.lsp4j.jsonrpc.services.JsonNotification

import java.lang.{Boolean => JBoolean, Integer => JInt}
import java.util.{List => JList}
import javax.annotation.Nullable
import org.eclipse.{lsp4j => l}
import org.eclipse.lsp4j.services.LanguageClient
import java.util.concurrent.CompletableFuture
import java.util.UUID

trait PlasmonLanguageClient extends LanguageClient {

  @JsonNotification("metals/executeClientCommand")
  def metalsExecuteClientCommand(params: l.ExecuteCommandParams): Unit

  def refreshModel(): CompletableFuture[Unit]

  def shutdown(): Unit = {}

  @JsonNotification("plasmon/statusUpdate")
  def statusUpdate(uri: String, updates: JList[PlasmonLanguageClient.StatusUpdate]): Unit

  @JsonNotification("plasmon/log")
  def log(message: PlasmonLanguageClient.LogMessage): Unit

  @JsonNotification("plasmon/heartBeat")
  def heartBeat(): Unit

  @JsonNotification("plasmon/progress")
  def progress(details: PlasmonLanguageClient.ProgressDetails): Unit

  @JsonNotification("plasmon/buildChangeDetected")
  def buildChangeDetected(details: PlasmonLanguageClient.BuildChangeDetails): Unit

  final def reportProgress[T](
    buildToolId: String,
    buildToolName: String,
    request: String,
    requestId: String = UUID.randomUUID().toString
  )(action: => T): T = {
    val details = PlasmonLanguageClient.ProgressDetails(
      buildToolId,
      buildToolName,
      requestId,
      request,
      done = false
    )
    progress(details)
    try action
    finally progress(details.asDone)
  }
}

object PlasmonLanguageClient {
  final case class LogMessage(
    channelId: String,
    channelLabel: String,
    lines: JList[String]
  )

  final case class StatusUpdate(
    id: String,
    text: String,
    severity: JInt,
    busy: JBoolean,
    @Nullable
    detail: String = null,
    @Nullable
    command: Command = null
  )

  final case class Command(
    title: String,
    command: String,
    @Nullable
    tooltip: String = null,
    @Nullable
    arguments: JList[String] = null
  )

  final case class ProgressDetails(
    buildToolId: String,
    buildToolName: String,
    requestId: String,
    request: String,
    done: JBoolean
  ) {
    def asDone: ProgressDetails =
      copy(done = true)
  }

  final case class BuildChangeDetails()
}
