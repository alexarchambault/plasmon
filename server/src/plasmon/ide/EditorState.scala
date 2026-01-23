package plasmon.ide

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import plasmon.render.JsonCodecs.given

final class EditorState(
  val buffers: Buffers,
  var focusedDocument: Option[os.Path],
  val fingerprints: MutableMd5Fingerprints,
  val trees: Trees
) {
  def updateFocusedDocument(path: os.Path, onDisk: String, inEditor: String): Unit = {
    focusedDocument = Some(path)
    // Update md5 fingerprint from file contents on disk
    fingerprints.add(path, onDisk)
    // Update in-memory buffer contents from LSP client
    buffers.put(path, inEditor)
  }
  def closed(path: os.Path): Unit = {
    buffers.remove(path)
    trees.didClose(path)
  }

  def asJson: EditorState.AsJson =
    EditorState.AsJson(
      buffers = buffers.asJson,
      focusedDocument = focusedDocument,
      fingerprints = fingerprints.asJson,
      trees = trees.asJson
    )
}

object EditorState {
  final case class AsJson(
    buffers: Buffers.AsJson,
    focusedDocument: Option[os.Path],
    fingerprints: MutableMd5Fingerprints.AsJson,
    trees: Trees.AsJson
  )

  given JsonValueCodec[AsJson] =
    JsonCodecMaker.make
}
