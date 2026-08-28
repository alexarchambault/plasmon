// Originally based on https://github.com/scalameta/metals/blob/0ad0bc184f82dbd178d01f76913ea6bdfa98db14/metals/src/main/scala/scala/meta/internal/metals/clients/language/NoopLanguageClient.scala or earlier versions of that file

package plasmon.languageclient

import org.eclipse.lsp4j as l

import java.util.List as JList
import java.util.concurrent.CompletableFuture

abstract class PlasmonNoopLanguageClient extends PlasmonLanguageClient {
  override def telemetryEvent(`object`: Any): Unit = ()
  override def publishDiagnostics(diagnostics: l.PublishDiagnosticsParams): Unit =
    ()
  override def showMessage(messageParams: l.MessageParams): Unit = ()
  override def showMessageRequest(
    requestParams: l.ShowMessageRequestParams
  ): CompletableFuture[l.MessageActionItem] =
    new CompletableFuture[l.MessageActionItem]()
  override def logMessage(message: l.MessageParams): Unit = ()
  override def executeClientCommand(params: l.ExecuteCommandParams): Unit =
    ()

  override def refreshModel(): CompletableFuture[Unit] =
    CompletableFuture.completedFuture(())

  override def refreshCodeLenses(): CompletableFuture[Void] =
    CompletableFuture.completedFuture(null)

  override def statusUpdate(uri: String, update: JList[PlasmonLanguageClient.StatusUpdate]): Unit =
    ()
  override def log(message: PlasmonLanguageClient.LogMessage): Unit = ()
  override def heartBeat(): Unit                                    = ()

  override def progress(details: PlasmonLanguageClient.ProgressDetails): Unit = ()

  override def buildChangeDetected(details: PlasmonLanguageClient.BuildChangeDetails): Unit = ()

  /** There is no editor to hand the edit to.
    *
    * A server running without an LSP client at all (`plasmon server --lsp=false`) ends up here;
    * `plasmon lsp did-open` and friends carry out the edits they cause themselves rather than going
    * through a client, so this is the incidental caller rather than the usual one.
    */
  override def applyEdit(params: l.ApplyWorkspaceEditParams)
    : CompletableFuture[l.ApplyWorkspaceEditResponse] = {
    scribe.warn(s"No client to apply an edit, ignoring it: $params")
    CompletableFuture.completedFuture(new l.ApplyWorkspaceEditResponse(false))
  }
}

object PlasmonNoopLanguageClient extends PlasmonNoopLanguageClient
