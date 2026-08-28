package plasmon.integration

import io.github.alexarchambault.testutil.TestUtil.*
import org.eclipse.lsp4j as l
import org.eclipse.lsp4j.jsonrpc.services.JsonNotification
import org.eclipse.lsp4j.services.LanguageClient

import java.io.OutputStream
import java.net.URI
import java.nio.file.Paths
import java.util.List as JList
import java.util.concurrent.CompletableFuture

import scala.collection.mutable
import scala.concurrent.duration.FiniteDuration

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
  private val appliedEdits0       = new mutable.ListBuffer[l.WorkspaceEdit]

  override def logMessage(params: l.MessageParams): Unit =
    lock.synchronized {
      outputStream.pprint(params)
      logMessages0 += params
    }
  override def publishDiagnostics(params: l.PublishDiagnosticsParams): Unit =
    lock.synchronized {
      outputStream.pprint(params)
      publishDiagnostics0 += params
      lock.notifyAll()
    }

  /** Waits for the server to publish diagnostics for `path`, and returns the first non-empty
    * publication for it.
    */
  def awaitDiagnostics(
    path: os.Path,
    timeout: FiniteDuration
  ): Option[l.PublishDiagnosticsParams] = {
    val deadline = System.currentTimeMillis() + timeout.toMillis
    lock.synchronized {
      def found = publishDiagnostics0.find { params =>
        !params.getDiagnostics.isEmpty &&
        os.Path(Paths.get(new URI(params.getUri))) == path
      }
      var res       = found
      var remaining = deadline - System.currentTimeMillis()
      while (res.isEmpty && remaining > 0L) {
        lock.wait(remaining)
        res = found
        remaining = deadline - System.currentTimeMillis()
      }
      res
    }
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

  /** Carries out what the server asks for, the way an editor would.
    *
    * The `plasmon lsp did-open` / `did-change` commands do the same thing on the other side of the
    * fence (see `plasmon.servercommand.WorkspaceEdits`), which is what lets a test drive the server
    * either way and end up looking at the same files.
    */
  override def applyEdit(params: l.ApplyWorkspaceEditParams)
    : CompletableFuture[l.ApplyWorkspaceEditResponse] =
    lock.synchronized {
      outputStream.pprint(params)
      WorkspaceEdits.applyToDisk(params.getEdit)
      appliedEdits0 += params.getEdit
      lock.notifyAll()
      CompletableFuture.completedFuture(new l.ApplyWorkspaceEditResponse(true))
    }

  /** How many workspace edits the server has asked for so far. */
  def appliedEditCount: Int =
    lock.synchronized(appliedEdits0.length)

  /** Waits for `count` workspace edits beyond the `since` already seen, and returns those.
    *
    * Counting from a mark rather than from zero keeps two document events in the same test from
    * being told apart only by luck.
    */
  def awaitAppliedEdits(since: Int, count: Int, timeout: FiniteDuration): Seq[l.WorkspaceEdit] = {
    val deadline = System.currentTimeMillis() + timeout.toMillis
    lock.synchronized {
      var remaining = deadline - System.currentTimeMillis()
      while (appliedEdits0.length - since < count && remaining > 0L) {
        lock.wait(remaining)
        remaining = deadline - System.currentTimeMillis()
      }
      appliedEdits0.drop(since).toList
    }
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
