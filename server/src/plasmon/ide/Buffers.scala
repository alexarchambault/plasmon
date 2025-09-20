// Originally based on https://github.com/scalameta/metals/blob/2a6f8a437a1ce7c44140673edfe34bb74dfd33be/metals/src/main/scala/scala/meta/internal/metals/Buffers.scala

package plasmon.ide

import plasmon.PlasmonEnrichments._

import scala.collection.concurrent.TrieMap
import scala.meta.inputs.Input
import scala.meta.internal.mtags.SourcePath
import scala.meta.internal.mtags.GlobalSymbolIndex

/** Manages in-memory text contents of unsaved files in the editor.
  */
case class Buffers(
  map: TrieMap[os.Path, String] = TrieMap.empty
) {
  def open: Iterable[os.Path]                = map.keys
  def put(key: os.Path, value: String): Unit = map.put(key, value)
  def get(key: os.Path): Option[String]      = map.get(key)
  def remove(key: os.Path): Unit             = map.remove(key)
  def contains(key: os.Path): Boolean        = map.contains(key)

  def tokenEditDistance(
    module: GlobalSymbolIndex.Module,
    source: os.Path,
    snapshot: String,
    trees: Trees
  ): TokenEditDistance = {
    val bufferInput = source.toInputFromBuffers(this)
    tokenEditDistance0(module, bufferInput, snapshot, trees)
  }

  def tokenEditDistance(
    module: GlobalSymbolIndex.Module,
    source: SourcePath,
    snapshot: String,
    trees: Trees
  )(implicit ctx: SourcePath.Context): TokenEditDistance = {
    val bufferInput = source.toInputFromBuffers(this)
    tokenEditDistance0(module, bufferInput, snapshot, trees)
  }

  private def tokenEditDistance0(
    module: GlobalSymbolIndex.Module,
    bufferInput: Input.VirtualFile,
    snapshot: String,
    trees: Trees
  ): TokenEditDistance = {
    val snapshotInput = Input.VirtualFile(bufferInput.path, snapshot)
    TokenEditDistance(module, snapshotInput, bufferInput, trees).getOrElse(
      TokenEditDistance.NoMatch
    )
  }

}
