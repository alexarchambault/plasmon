package plasmon.command

import org.eclipse.lsp4j.jsonrpc as j
import org.eclipse.lsp4j.jsonrpc.Launcher
import plasmon.internal.DebugInput
import plasmon.protocol.CommandClient
import plasmon.servercommand.ServerCommandInstance
import plasmon.util.ThreadUtil

import java.nio.channels.{ServerSocketChannel, SocketChannel}
import java.util.concurrent.ExecutorService

import scala.util.control.NonFatal

/** Accepts command connections - one per `plasmon …` invocation - and runs the commands they send.
  *
  * Each connection gets its own [[CommandServerImpl]] and [[ConnectionCommandClient]], so that
  * concurrent invocations each get their own output, and so that the commands of an invocation that
  * went away fail rather than wait on it forever.
  *
  * Both pools are cached rather than fixed: each connection holds a JSON-RPC thread for as long as
  * it is open, and a fixed number of those would leave connections beyond it unread.
  */
object CommandSocketServer {

  /** Accepts connections until the thread is interrupted (or the channel closed) */
  def run(
    serverChannel: ServerSocketChannel,
    remoteCommands: CommandClient => Seq[ServerCommandInstance[?]],
    logJsonrpc: Boolean
  ): Unit =
    ThreadUtil.withCachedThreadPool("plasmon-commands-jsonrpc") { pool =>
      ThreadUtil.withCachedThreadPool("plasmon-commands") { runCommandPool =>
        while (true) {
          val clientSocketChannel = serverChannel.accept()
          try serve(clientSocketChannel, remoteCommands, logJsonrpc, pool, runCommandPool)
          catch {
            case NonFatal(e) =>
              scribe.warn("Error setting up command connection", e)
              try clientSocketChannel.close()
              catch {
                case NonFatal(_) =>
              }
          }
        }
      }
    }

  private def serve(
    clientSocketChannel: SocketChannel,
    remoteCommands: CommandClient => Seq[ServerCommandInstance[?]],
    logJsonrpc: Boolean,
    pool: ExecutorService,
    runCommandPool: ExecutorService
  ): Unit = {
    val clientSocket = libdaemonjvm.Util.socketFromChannel(clientSocketChannel)

    val (input, output) =
      if (logJsonrpc)
        DebugInput.debug(
          clientSocket.getInputStream,
          clientSocket.getOutputStream,
          (line, isOut) => scribe.info(s"Command server ${if (isOut) ">" else "<"} $line")
        )
      else
        (clientSocket.getInputStream, clientSocket.getOutputStream)

    val client = new ConnectionCommandClient

    val launcher = new Launcher.Builder[CommandClient]()
      .setExecutorService(pool)
      .setInput(input)
      .setOutput(output)
      .setRemoteInterface(classOf[CommandClient])
      .setLocalService(new CommandServerImpl(client, remoteCommands, runCommandPool))
      .setExceptionHandler { t =>
        scribe.info("Error during command processing", t)
        j.RemoteEndpoint.DEFAULT_EXCEPTION_HANDLER.apply(t)
      }
      .create()

    client.connect(launcher.getRemoteProxy)

    val listening = launcher.startListening()

    // Listening stops once the client closed its end (it exited, most likely). Anything still
    // waiting on it would wait forever otherwise, see ConnectionCommandClient.
    pool.execute { () =>
      try listening.get()
      catch {
        case NonFatal(_) =>
      }
      finally {
        client.close()
        try clientSocket.close()
        catch {
          case NonFatal(_) =>
        }
      }
    }
  }
}
