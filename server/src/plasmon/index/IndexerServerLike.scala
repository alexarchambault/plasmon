package plasmon.index

import plasmon.ServerThreadPools
import plasmon.bsp.BspServers
import plasmon.ide.*
import plasmon.languageclient.PlasmonLanguageClient
import plasmon.pc.PresentationCompilers
import plasmon.watch.FileWatcher

import scala.meta.internal.metals.Docstrings
import scala.meta.internal.mtags.{OnDemandSymbolIndex, SourcePath}
import scala.meta.internal.pc.CustomFileManager

trait IndexerServerLike {
  def workingDir: os.Path
  def jdkCp: Seq[os.Path]
  def jdkSources: Option[os.Path]

  /** Manages BSP servers (build tools) */
  def bspServers: BspServers

  /** Cached BSP data from build tools */
  def bspData: BspData

  /** Symbol index - returns symbol's position, sources, … */
  def symbolIndex: OnDemandSymbolIndex

  /** ??? */
  def symbolSearchIndex: SymbolSearchIndex

  /** Symbol documentation index - returns symbol's javadoc / scaladoc */
  def symbolDocs: Docstrings
  def presentationCompilers: PresentationCompilers
  def compilations: Compilations

  def editorState: EditorState
  def fileWatcher: FileWatcher

  def readAllSemanticdbs(): Unit
  def resetCaches(): Unit

  def pools: ServerThreadPools
  def languageClient: PlasmonLanguageClient
}
