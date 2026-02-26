package plasmon.command

import caseapp.core.RemainingArgs
import org.eclipse.lsp4j as l
import org.eclipse.lsp4j.jsonrpc.Launcher
import plasmon.internal.{DebugInput, Directories}
import plasmon.protocol.{Command as ProtocolCommand, *}
import plasmon.util.ThreadUtil

import java.net.{StandardProtocolFamily, UnixDomainSocketAddress}
import java.nio.channels.SocketChannel
import java.util.concurrent.{CompletableFuture, LinkedBlockingQueue, TimeUnit}

import scala.concurrent.Promise
import scala.util.{Properties, Success}

object Command extends caseapp.Command[CommandOptions] {

  def socketPath = os.sub / ".plasmon/socket"

  def actualSocket(basePath: os.Path, isServer: Boolean = false): os.Path =
    if (Properties.isWin)
      basePath
    else if (isServer) {
      val directories = new Directories
      val dir = os.Path(
        if (Properties.isMac) directories.cacheDir() // shorter path, which helps us here
        else directories.dataDir(),
        os.pwd
      )
      val pid  = ProcessHandle.current().pid()
      val path = dir / "sockets" / pid.toString
      os.write.over(basePath, path.toString, createFolders = true)
      path
    }
    else
      os.Path(os.read(basePath))

  override def stopAtFirstUnrecognized = true
  def run(options: CommandOptions, remainingArgs: RemainingArgs): Unit = {

    val workingDir = options.workingDir
      .filter(_.trim.nonEmpty)
      .map(os.Path(_, os.pwd))
      .getOrElse(os.pwd)

    val socketPath0 =
      options.socket.filter(_.trim.nonEmpty)
        .map(os.Path(_, os.pwd))
        .getOrElse(actualSocket(workingDir / socketPath))

    if (options.verbosity >= 1)
      System.err.println(s"Connecting to plasmon server via socket $socketPath0")

    if (!os.exists(socketPath0)) {
      System.err.println(s"$socketPath0 not found")
      sys.exit(1)
    }

    val addr          = UnixDomainSocketAddress.of(socketPath0.toNIO)
    val socketChannel = SocketChannel.open(StandardProtocolFamily.UNIX)
    if (options.verbosity >= 1)
      System.err.println("Connecting...")
    socketChannel.connect(addr)
    socketChannel.finishConnect()
    if (options.verbosity >= 1)
      System.err.println("Connected")

    val queue = new LinkedBlockingQueue[(String, Boolean, Promise[Unit])]
    val poisonPill: (String, Boolean, Promise[Unit]) = (null, false, null)

    val outputThread: Thread =
      new Thread("output-thread") {
        setDaemon(true)
        override def run(): Unit =
          try {
            if (options.verbosity >= 2)
              System.err.println("Output thread starting")
            var done = false
            while (!done) {
              val elemOrNull = queue.poll(10L, TimeUnit.SECONDS)
              if (elemOrNull == poisonPill)
                done = true
              else if (elemOrNull != null) {
                val (line, isStderr, promise) = elemOrNull
                if (isStderr)
                  System.err.println(line)
                else
                  println(line)
                promise.tryComplete(Success(()))
              }
            }
          }
          finally
            if (options.verbosity >= 2)
              System.err.println("Output thread exiting")
      }

    outputThread.start()

    val client: CommandClient = new CommandClientImpl(queue)

    val socket = libdaemonjvm.Util.socketFromChannel(socketChannel)

    try
      ThreadUtil.withFixedThreadPool("plasmon-command-jsonrpc", 4) { pool =>

        val (input, output) =
          if (options.logJsonrpcInput.getOrElse(false))
            DebugInput.debug(
              socket.getInputStream,
              socket.getOutputStream,
              (line, isOut) => scribe.info(s"Command client ${if (isOut) ">" else "<"} $line")
            )
          else
            (socket.getInputStream, socket.getOutputStream)

        val launcher = new Launcher.Builder[CommandServer]()
          .setExecutorService(pool)
          .setInput(input)
          .setOutput(output)
          .setRemoteInterface(classOf[CommandServer])
          .setLocalService(client)
          .setExceptionHandler { t =>
            scribe.info("Error during command processing", t)
            l.jsonrpc.RemoteEndpoint.DEFAULT_EXCEPTION_HANDLER.apply(t)
          }
          .create()

        val remoteServer = launcher.getRemoteProxy
        // client.setServer(remoteServer)

        if (options.verbosity >= 2)
          System.err.println("Starting JSON-RPC exchange")
        launcher.startListening()

        if (options.verbosity >= 1)
          System.err.println(s"Running command ${remainingArgs.all.mkString(" ")} via JSON-RPC")
        val res = remoteServer.runCommand {
          val command = new ProtocolCommand
          command.setArgs(remainingArgs.all.toArray)
          command
        }.get()

        val exitCode = res.getExitCode
        if (options.verbosity >= 1)
          System.err.println(s"Done running command (exit code: $exitCode)")
        if (exitCode != 0)
          sys.exit(exitCode)
      }
    finally {
      queue.add(poisonPill)
      outputThread.join()
      // This prints garbage in the console. It's unclear to me how we can stop the JSON-RPC stuff.
      // socket.close()
    }
  }
}
