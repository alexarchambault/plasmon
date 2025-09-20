package plasmon.bsp

import plasmon.Logger
import plasmon.languageclient.PlasmonLanguageClient

trait BspServersServerLike {
  def javaHome: os.Path
  def logJsonrpcInput: Boolean
  def enableBestEffortMode: Boolean
  def bloopJavaHome: () => os.Path

  def workingDir: os.Path
  def loggerManager: Logger.Manager

  def tools: BuildTool.Tools

  def createBuildClient(): PlasmonBuildClientImpl
  def languageClient: PlasmonLanguageClient
}
