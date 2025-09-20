package plasmon.integration

import io.github.alexarchambault.testutil.TestUtil.*
import org.eclipse.lsp4j.jsonrpc.services.JsonNotification
import org.eclipse.lsp4j.services.LanguageClient
import org.eclipse.{lsp4j => l}

import java.io.OutputStream
import java.util.{List => JList}
import java.util.concurrent.CompletableFuture

import scala.collection.mutable

// This needs to be a trait. The lsp4j reflection stuff is unhappy if it's a class
// (it finds duplicated methods…)
trait MockLanguageClient extends LanguageClient with MockLanguageClient.Stuff {

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
}
