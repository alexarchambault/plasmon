// Originally based on https://github.com/scalameta/metals/blob/0ad0bc184f82dbd178d01f76913ea6bdfa98db14/metals/src/main/scala/scala/meta/internal/metals/clients/language/ConfiguredLanguageClient.scala or earlier versions of that file

package plasmon.languageclient

import java.util.concurrent.CompletableFuture
import java.util.{List => JList}

import scala.concurrent.ExecutionContext
import org.eclipse.{lsp4j => l}
import java.util.concurrent.atomic.AtomicBoolean
import plasmon.PlasmonEnrichments.*

class PlasmonConfiguredLanguageClient(
  initial: PlasmonLanguageClient,
  initParams: l.InitializeParams
)(implicit ec: ExecutionContext) extends PlasmonDelegatingLanguageClient(initial)
    with PlasmonLanguageClient {

  override def shutdown(): Unit = {
    underlying = PlasmonNoopLanguageClient
  }

  override def showMessage(params: l.MessageParams): Unit =
    underlying.showMessage(params)

  private[plasmon] val pendingShowMessage = new AtomicBoolean(false)
  override def showMessageRequest(params: l.ShowMessageRequestParams)
    : CompletableFuture[l.MessageActionItem] = {
    pendingShowMessage.set(true)
    val result = underlying.showMessageRequest(params)
    result.asScala.onComplete(_ => pendingShowMessage.set(false))
    result
  }

  override def logMessage(message: l.MessageParams): Unit =
    underlying.logMessage(message)

  override def refreshModel(): CompletableFuture[Unit] =
    if (initParams.codeLensRefreshSupport)
      underlying.refreshCodeLenses.thenApply(_ => ())
    else CompletableFuture.completedFuture(())

  override def metalsExecuteClientCommand(params: l.ExecuteCommandParams): Unit =
    underlying.metalsExecuteClientCommand(params)

  override def statusUpdate(uri: String, updates: JList[PlasmonLanguageClient.StatusUpdate]): Unit =
    underlying.statusUpdate(uri, updates)

  override def log(message: PlasmonLanguageClient.LogMessage): Unit =
    underlying.log(message)

  override def heartBeat(): Unit =
    underlying.heartBeat()

  override def progress(details: PlasmonLanguageClient.ProgressDetails): Unit =
    underlying.progress(details)

  override def buildChangeDetected(details: PlasmonLanguageClient.BuildChangeDetails): Unit =
    underlying.buildChangeDetected(details)

  override def applyEdit(params: l.ApplyWorkspaceEditParams)
    : CompletableFuture[l.ApplyWorkspaceEditResponse] =
    if (initParams.applyEditSupport)
      initial.applyEdit(params)
    else {
      scribe.warn("applyEdit not supported by client, cannot send patch")
      CompletableFuture.completedFuture(new l.ApplyWorkspaceEditResponse(false))
    }

}
