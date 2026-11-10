package plasmon.remotepc.host

import java.io.PrintStream
import java.util.concurrent.{Executors, ThreadFactory}
import java.util.concurrent.atomic.AtomicInteger

import org.eclipse.lsp4j.jsonrpc.Launcher
import plasmon.remotepc.RemotePcClient

/** Runs Scala 2 presentation compilers for a Plasmon server, that talks to us with JSON-RPC over
  * our standard input and output.
  *
  * Exits once our standard input is closed, so that we don't outlive the server.
  */
object Scala2PcHost {

  private[host] def daemonThreadFactory(prefix: String): ThreadFactory = {
    val count = new AtomicInteger
    (r: Runnable) => {
      val t = new Thread(r, s"$prefix-${count.incrementAndGet()}")
      t.setDaemon(true)
      t
    }
  }

  def main(args: Array[String]): Unit = {
    // Our standard output is the JSON-RPC channel: nothing else must write to it
    val out = System.out
    System.setOut(new PrintStream(System.err, true))

    val server = new HostServer
    val launcher = new Launcher.Builder[RemotePcClient]()
      .setLocalService(server)
      .setRemoteInterface(classOf[RemotePcClient])
      .setInput(System.in)
      .setOutput(out)
      .setExecutorService(Executors.newCachedThreadPool(daemonThreadFactory("jsonrpc")))
      .create()
    server.setClient(launcher.getRemoteProxy)

    try launcher.startListening().get()
    finally {
      server.shutdownAll()
      sys.exit(0)
    }
  }
}
