package plasmon.command

import caseapp.core.RemainingArgs
import org.eclipse.lsp4j as l
import org.eclipse.lsp4j.jsonrpc.Launcher
import plasmon.internal.{DebugInput, Directories}
import plasmon.protocol.{Command as ProtocolCommand, *}
import plasmon.util.ThreadUtil

import java.io.{FileDescriptor, FileOutputStream, IOException, PrintStream}
import java.net.{StandardProtocolFamily, UnixDomainSocketAddress}
import java.nio.channels.SocketChannel
import java.nio.charset.StandardCharsets
import java.util.concurrent.{LinkedBlockingQueue, TimeUnit}

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

  /** What the server sends us, written out as UTF-8 rather than in the platform default encoding.
    *
    * `System.out` picks up the console encoding, which on a machine with no UTF-8 locale (a CI
    * container, say) turns anything outside ASCII into `?`. That is lossy for any output, and it
    * makes the JSON the `--json` flags print unparseable for whoever asked for it.
    */
  private def utf8(fd: FileDescriptor): PrintStream =
    new PrintStream(new FileOutputStream(fd), true, StandardCharsets.UTF_8)

  def run(options: CommandOptions, remainingArgs: RemainingArgs): Unit = {

    val workingDir = options.workingDir
      .filter(_.trim.nonEmpty)
      .map(os.Path(_, os.pwd))
      .getOrElse(os.pwd)

    val explicitSocketOpt = options.socket.filter(_.trim.nonEmpty).map(os.Path(_, os.pwd))
    val basePath          = workingDir / socketPath

    // Where a server would be listening, if one is - `None` when nothing wrote it down, which is
    // the answer whenever nobody started a server here
    def currentSocketPathOpt(): Option[os.Path] =
      explicitSocketOpt.orElse {
        Option.when(os.exists(basePath))(actualSocket(basePath))
      }

    /** Connects to whatever server is listening, hands back the socket it was found on.
      *
      * `None` covers both "nothing to connect to" and "what was written down doesn't answer" - a
      * server that died leaves its path behind either way, and both are equally a reason to start
      * one when `--auto` says to.
      */
    def tryConnect(): Option[(os.Path, SocketChannel)] =
      currentSocketPathOpt()
        .filter(os.exists(_))
        .flatMap { socketPath0 =>
          if (options.verbosity >= 1)
            System.err.println(s"Connecting to plasmon server via socket $socketPath0")
          val socketChannel = SocketChannel.open(StandardProtocolFamily.UNIX)
          try {
            socketChannel.connect(UnixDomainSocketAddress.of(socketPath0.toNIO))
            socketChannel.finishConnect()
            if (options.verbosity >= 1)
              System.err.println("Connected")
            Some((socketPath0, socketChannel))
          }
          catch {
            case e: IOException =>
              if (options.verbosity >= 1)
                System.err.println(s"Could not connect to $socketPath0: $e")
              socketChannel.close()
              None
          }
        }

    val socketChannel = tryConnect().map(_._2).getOrElse {
      if (AutoServer.requested(remainingArgs.all))
        AutoServer.startAndConnect(
          workingDir,
          explicitSocketOpt,
          options.verbosity,
          () => tryConnect()
        )
      else {
        // "No server here" is what this is, whenever someone runs a command before starting one -
        // an exception from `actualSocket` or from the connection itself would only hide it
        val message = currentSocketPathOpt() match {
          case None =>
            s"$basePath not found, is a plasmon server running in $workingDir?"
          case Some(socketPath0) if !os.exists(socketPath0) =>
            s"$socketPath0 not found"
          case Some(socketPath0) =>
            s"Cannot connect to $socketPath0, is a plasmon server still running in $workingDir?"
        }
        // Where --auto would have started one - having no server is exactly the moment someone
        // finds out they wanted it
        val hint =
          if (remainingArgs.all.headOption.exists(_.startsWith("lsp")))
            " Pass --auto to start one."
          else
            ""
        System.err.println(message + hint)
        sys.exit(1)
      }
    }

    val queue = new LinkedBlockingQueue[(String, Boolean, Promise[Unit])]
    val poisonPill: (String, Boolean, Promise[Unit]) = (null, false, null)

    val stdout = utf8(FileDescriptor.out)
    val stderr = utf8(FileDescriptor.err)

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
                  stderr.println(line)
                else
                  stdout.println(line)
                promise.tryComplete(Success(()))
              }
            }
          }
          finally {
            stdout.flush()
            stderr.flush()
            if (options.verbosity >= 2)
              System.err.println("Output thread exiting")
          }
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
