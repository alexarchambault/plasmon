package plasmon.integration

import io.github.alexarchambault.testutil.TestOutput.FixedReadBytes

import java.io.OutputStream
import java.net.{StandardProtocolFamily, UnixDomainSocketAddress}
import java.nio.ByteBuffer
import java.nio.channels.{ServerSocketChannel, SocketChannel}

import scala.concurrent.{Await, Promise}
import scala.concurrent.duration.FiniteDuration
import scala.util.control.NonFatal

/** A BSP server listening on a UNIX domain socket, the way one started by someone else would be.
  *
  * Build tools speak BSP over their standard input and output, so this listens on `socket` and, for
  * each connection, starts `command` in the workspace and pipes bytes between the two. The
  * workspace is only known once the test harness has created it, hence [[setWorkspace]]:
  * connections accepted before that wait for it.
  */
final class BspSocketBridge(
  socket: os.Path,
  command: os.Path => Seq[os.Shellable],
  err: Option[OutputStream],
  workspaceTimeout: FiniteDuration
) extends AutoCloseable {

  private val workspacePromise = Promise[os.Path]()
  def setWorkspace(workspace: os.Path): Unit =
    workspacePromise.trySuccess(workspace)

  private val serverChannel = ServerSocketChannel.open(StandardProtocolFamily.UNIX)
  serverChannel.bind(UnixDomainSocketAddress.of(socket.toNIO))

  @volatile private var closed        = false
  @volatile private var processes     = List.empty[os.SubProcess]
  @volatile private var channels      = List.empty[SocketChannel]
  @volatile private var acceptedCount = 0

  /** How many connections were accepted so far */
  def connectionCount: Int = acceptedCount

  private def thread(name: String)(f: => Unit): Thread = {
    val t = new Thread(name) {
      setDaemon(true)
      override def run(): Unit =
        try f
        catch {
          case NonFatal(e) if closed =>
          case NonFatal(e) =>
            for (out <- err) {
              out.write(s"Error in $name: $e${System.lineSeparator()}".getBytes)
              out.flush()
            }
        }
    }
    t.start()
    t
  }

  private def serve(channel: SocketChannel): Unit = {
    val workspace = Await.result(workspacePromise.future, workspaceTimeout)
    val proc = os.proc(command(workspace)).spawn(
      cwd = workspace,
      stderr = FixedReadBytes.pipeTo(err)
    )
    synchronized {
      processes = proc :: processes
    }
    // Reading and writing a SocketChannel from two threads is fine, they don't share a lock
    thread("bsp-bridge-to-server") {
      val buf = ByteBuffer.allocate(8192)
      while (channel.read(buf) >= 0) {
        buf.flip()
        proc.stdin.write(buf.array(), 0, buf.limit())
        proc.stdin.flush()
        buf.clear()
      }
      proc.stdin.close()
    }
    thread("bsp-bridge-to-client") {
      val buf  = new Array[Byte](8192)
      var read = 0
      while ({ read = proc.stdout.read(buf); read >= 0 }) {
        val bb = ByteBuffer.wrap(buf, 0, read)
        while (bb.hasRemaining)
          channel.write(bb)
      }
      channel.close()
    }
  }

  thread("bsp-bridge-accept") {
    while (!closed) {
      val channel = serverChannel.accept()
      synchronized {
        channels = channel :: channels
        acceptedCount += 1
      }
      thread("bsp-bridge-serve")(serve(channel))
    }
  }

  def close(): Unit = {
    closed = true
    serverChannel.close()
    for (channel <- channels)
      try channel.close()
      catch { case NonFatal(_) => }
    for (proc <- processes) {
      proc.destroy()
      if (!proc.waitFor(5000L))
        proc.destroyForcibly()
    }
    os.remove(socket)
  }
}
