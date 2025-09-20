// Originally based on https://github.com/scalameta/metals/blob/0ad0bc184f82dbd178d01f76913ea6bdfa98db14/metals/src/main/scala/scala/meta/internal/metals/watcher/FileWatcher.scala

package plasmon.watch

trait FileWatcher extends AutoCloseable {
  def start(): Unit

  def suspend(): Unit
  def resume(): Unit
  def enqueue(event: WatchEvent): Unit
}

object NoopFileWatcher extends FileWatcher {
  def start(): Unit                    = {}
  def close(): Unit                    = {}
  def suspend(): Unit                  = {}
  def resume(): Unit                   = {}
  def enqueue(event: WatchEvent): Unit = {}
}
