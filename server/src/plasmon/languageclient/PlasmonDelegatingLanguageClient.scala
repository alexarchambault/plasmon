// Originally based on https://github.com/scalameta/metals/blob/0ad0bc184f82dbd178d01f76913ea6bdfa98db14/metals/src/main/scala/scala/meta/internal/metals/clients/language/DelegatingLanguageClient.scala or earlier versions of that file

package plasmon.languageclient

import org.eclipse.lsp4j as l

import java.util.List as JList
import java.util.concurrent.CompletableFuture

class PlasmonDelegatingLanguageClient(var underlying: PlasmonLanguageClient)
    extends PlasmonLanguageClient {

  override def shutdown(): Unit =
    underlying.shutdown()

  override def registerCapability(params: l.RegistrationParams): CompletableFuture[Void] =
    underlying.registerCapability(params)

  override def unregisterCapability(params: l.UnregistrationParams): CompletableFuture[Void] =
    underlying.unregisterCapability(params)

  override def applyEdit(params: l.ApplyWorkspaceEditParams)
    : CompletableFuture[l.ApplyWorkspaceEditResponse] =
    underlying.applyEdit(params)

  override def telemetryEvent(value: Any): Unit =
    underlying.telemetryEvent(value)

  override def publishDiagnostics(diagnostics: l.PublishDiagnosticsParams): Unit =
    underlying.publishDiagnostics(diagnostics)

  override def showMessage(params: l.MessageParams): Unit =
    underlying.showMessage(params)

  override def showMessageRequest(params: l.ShowMessageRequestParams)
    : CompletableFuture[l.MessageActionItem] =
    underlying.showMessageRequest(params)

  override def logMessage(message: l.MessageParams): Unit =
    underlying.logMessage(message)

  override def metalsExecuteClientCommand(params: l.ExecuteCommandParams): Unit =
    underlying.metalsExecuteClientCommand(params)

  override def refreshModel(): CompletableFuture[Unit] =
    underlying.refreshModel()

  override def configuration(configurationParams: l.ConfigurationParams)
    : CompletableFuture[JList[Object]] =
    underlying.configuration(configurationParams)

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
}
