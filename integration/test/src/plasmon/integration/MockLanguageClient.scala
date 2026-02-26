package plasmon.integration

import io.github.alexarchambault.testutil.TestUtil.*
import org.eclipse.lsp4j as l
import org.eclipse.lsp4j.jsonrpc.services.JsonNotification
import org.eclipse.lsp4j.services.LanguageClient

import java.io.OutputStream
import java.util.List as JList
import java.util.concurrent.CompletableFuture

import scala.collection.mutable

// This needs to be a trait. The lsp4j reflection stuff is unhappy if it's a class
// (it finds duplicated methods…)
trait MockLanguageClient extends LanguageClient with MockLanguageClient.Stuff
    with MockLanguageClient.NoAnnotationsOverrides {

  private var outputStream0: OutputStream = System.err
  def outputStream: OutputStream          = outputStream0
  def setOutputStream(os: OutputStream): Unit = {
    outputStream0 = os
  }

  private val lock = new Object

  private val logMessages0        = new mutable.ListBuffer[l.MessageParams]
  private val publishDiagnostics0 = new mutable.ListBuffer[l.PublishDiagnosticsParams]
  private val showMessage0        = new mutable.ListBuffer[l.MessageParams]
  private val showMessageRequest0 = new mutable.ListBuffer[l.ShowMessageRequestParams]
  private val telemetryEvent0     = new mutable.ListBuffer[Object]

  override def logMessage(params: l.MessageParams): Unit =
    lock.synchronized {
      outputStream.pprint(params)
      logMessages0 += params
    }
  override def publishDiagnostics(params: l.PublishDiagnosticsParams): Unit =
    lock.synchronized {
      outputStream.pprint(params)
      publishDiagnostics0 += params
    }
  override def showMessage(params: l.MessageParams): Unit =
    lock.synchronized {
      outputStream.pprint(params)
      showMessage0 += params
    }
  override def showMessageRequest(params: l.ShowMessageRequestParams)
    : CompletableFuture[l.MessageActionItem] =
    lock.synchronized {
      outputStream.pprint(params)
      showMessageRequest0 += params
      CompletableFuture.completedFuture(new l.MessageActionItem(""))
    }
  override def telemetryEvent(event: Object): Unit =
    lock.synchronized {
      outputStream.pprint(event)
      telemetryEvent0 += event
    }

  def plasmonLog(message: Object): Unit = ()

  def statusUpdate(uri: String, updates: JList[Object]): Unit = ()

  def progress(details: Object): Unit = ()
}

object MockLanguageClient {
  trait Stuff {
    @JsonNotification("plasmon/log")
    def plasmonLog(message: Object): Unit
    @JsonNotification("plasmon/statusUpdate")
    def statusUpdate(uri: String, updates: JList[Object]): Unit
    @JsonNotification("plasmon/progress")
    def progress(details: Object): Unit
  }

  // without this, it seems Scala 3 adds the same kind of thing automatically,
  // but adding back the original annotations, which confuses lsp4j (that complains
  // about duplicated stuff)
  trait NoAnnotationsOverrides extends LanguageClient {
    override def showDocument(params: l.ShowDocumentParams)
      : CompletableFuture[l.ShowDocumentResult] =
      super.showDocument(params)
    override def createProgress(params: l.WorkDoneProgressCreateParams): CompletableFuture[Void] =
      super.createProgress(params)
    override def notifyProgress(params: l.ProgressParams): Unit =
      super.notifyProgress(params)
    override def logTrace(params: l.LogTraceParams): Unit =
      super.logTrace(params)
    override def registerCapability(params: l.RegistrationParams): CompletableFuture[Void] =
      super.registerCapability(params)
    override def unregisterCapability(params: l.UnregistrationParams): CompletableFuture[Void] =
      super.unregisterCapability(params)
    override def workspaceFolders(): CompletableFuture[JList[l.WorkspaceFolder]] =
      super.workspaceFolders()
    override def configuration(configurationParams: l.ConfigurationParams)
      : CompletableFuture[JList[Object]] =
      super.configuration(configurationParams)
    override def refreshSemanticTokens(): CompletableFuture[Void] =
      super.refreshSemanticTokens()
    override def refreshCodeLenses(): CompletableFuture[Void] =
      super.refreshCodeLenses()
    override def refreshDiagnostics(): CompletableFuture[Void] =
      super.refreshDiagnostics()
    override def refreshInlayHints(): CompletableFuture[Void] =
      super.refreshInlayHints()
    override def refreshInlineValues(): CompletableFuture[Void] =
      super.refreshInlineValues()
    override def applyEdit(applyEditParams: l.ApplyWorkspaceEditParams)
      : CompletableFuture[l.ApplyWorkspaceEditResponse] =
      super.applyEdit(applyEditParams)
    override def refreshFoldingRanges(): CompletableFuture[Void] =
      super.refreshFoldingRanges()
    override def refreshTextDocumentContent(params: l.TextDocumentContentRefreshParams)
      : CompletableFuture[Void] =
      super.refreshTextDocumentContent(params)
  }
}
