package plasmon.ide

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
}
