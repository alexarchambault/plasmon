package plasmon.bsp

import ch.epfl.scala.bsp4j as b
import org.eclipse.lsp4j.jsonrpc.services.JsonNotification
import plasmon.Logger

trait PlasmonBuildClient {

  @JsonNotification("build/taskStart")
  def buildTaskStart(params: b.TaskStartParams): Unit
  @JsonNotification("build/taskProgress")
  def buildTaskProgress(params: b.TaskProgressParams): Unit
  @JsonNotification("build/taskFinish")
  def buildTaskFinish(params: b.TaskFinishParams): Unit

  def addClient(originId: String, client: PlasmonBuildClient.CompilationClient): Unit
  def removeClient(originId: String): Unit
  def progressFor(uri: String): Option[(Long, Long)]

  def setLogger(logger: Logger): Unit
  def logger: Option[Logger]
}

object PlasmonBuildClient {
  trait CompilationClient {
    def onBuildPublishDiagnostics(params: b.PublishDiagnosticsParams): Unit
  }
}
