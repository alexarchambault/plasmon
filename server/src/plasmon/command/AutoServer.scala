package plasmon.command

import plasmon.internal.BinaryName
import plasmon.servercommand.HasAutoOption

import java.io.File
import java.nio.channels.SocketChannel

import scala.collection.mutable.ListBuffer
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.util.Properties

/** Starting a server for a command that was passed `--auto`.
  *
  * `--auto` (see [[plasmon.servercommand.AutoLoad]]) is about a request not needing anything to
  * have been set up by hand beforehand. A running server is the first of those things: in an editor
  * the extension started one long before anyone hovers, while from a terminal nobody has, and every
  * command then stops on "is a plasmon server running?". So `--auto` starts one here, in the
  * background, and waits for it to answer.
  *
  * The server stays up once the command is done - the next command finds it, and whatever `--auto`
  * loaded in it is still loaded. `plasmon exit` stops it.
  */
object AutoServer {

  /** How long we wait for a server we started to answer.
    *
    * Generous on purpose: the server binds its command socket only once it has initialized itself
    * on its working directory, which on a large workspace with persisted targets to import is not
    * instant.
    */
  private def startTimeout: FiniteDuration = 5.minutes

  private def stillWaitingEvery: FiniteDuration = 30.seconds

  /** Where the server we start writes what it would have written to the terminal. */
  def outputFile(workingDir: os.Path): os.Path =
    workingDir / ".plasmon/server-output"

  /** What the options of a command say about `--auto`.
    *
    * `supported` is what tells "no server, and none asked for" from "no server, and this command
    * has no way to ask for one" - only the first of the two is worth suggesting `--auto` for.
    */
  final case class Auto(requested: Boolean, supported: Boolean)

  object Auto {
    def of(options: Any): Auto =
      options match {
        case withAuto: HasAutoOption => Auto(requested = withAuto.auto, supported = true)
        case _                       => Auto(requested = false, supported = false)
      }
  }

  /** Starts a server in `workingDir`, and waits for it to accept a connection.
    *
    * `tryConnect` is what the caller does to reach a server - re-run here until it answers, since
    * the socket path it looks at is only written once the server is up. It hands back the socket it
    * connected to along with the connection, so that we can tell a server we started from one that
    * appeared meanwhile.
    */
  def startAndConnect(
    workingDir: os.Path,
    explicitSocketOpt: Option[os.Path],
    verbosity: Int,
    tryConnect: () => Option[(os.Path, SocketChannel)]
  ): SocketChannel = {

    val command = selfCommand().getOrElse {
      System.err.println(
        "Cannot work out how to start a plasmon server from here, " +
          s"start one with 'plasmon server --lsp=false' in $workingDir"
      )
      sys.exit(1)
    } ++ serverArgs(workingDir, explicitSocketOpt)

    val output = outputFile(workingDir)
    os.makeDir.all(output / os.up)

    System.err.println(s"Starting a plasmon server in $workingDir (output in $output)")
    if (verbosity >= 1)
      System.err.println(s"Server command: ${command.mkString(" ")}")

    // Nothing ties it to us: os-lib doesn't destroy what it spawned when we exit, and we are
    // about to - leaving the server up for the next command is the whole point
    val proc = os.proc(command).spawn(
      cwd = workingDir,
      stdout = os.PathAppendRedirect(output),
      stderr = os.PathAppendRedirect(output)
    )

    val deadline    = System.currentTimeMillis() + startTimeout.toMillis
    var lastMessage = System.currentTimeMillis()

    var channelOpt = Option.empty[SocketChannel]
    while (channelOpt.isEmpty)
      tryConnect() match {
        case Some((socketPath, channel)) =>
          if (proc.isAlive() && startedByUs(socketPath, proc, explicitSocketOpt))
            System.err.println(
              s"Plasmon server started (pid ${proc.wrapped.pid()}). " +
                "It stays running - stop it with 'plasmon exit'"
            )
          else {
            // Someone else's server got there first, and the one we started is now waiting on
            // .plasmon/lock for a server that isn't going away - it would sit there forever
            System.err.println("Another plasmon server answered first")
            if (proc.isAlive()) {
              System.err.println("Stopping the one we started")
              proc.destroy()
            }
          }
          channelOpt = Some(channel)
        case None =>
          if (!proc.isAlive()) {
            System.err.println(
              s"The plasmon server we started exited (exit code ${proc.exitCode()})"
            )
            printOutputTail(output)
            sys.exit(1)
          }
          if (System.currentTimeMillis() >= deadline) {
            System.err.println(
              s"The plasmon server we started in $workingDir is still not answering " +
                s"after $startTimeout, giving up (leaving it running - see $output)"
            )
            printOutputTail(output)
            sys.exit(1)
          }
          if (System.currentTimeMillis() - lastMessage >= stillWaitingEvery.toMillis) {
            System.err.println(s"Still waiting for the plasmon server to start (see $output)")
            lastMessage = System.currentTimeMillis()
          }
          Thread.sleep(200L)
      }

    channelOpt.get
  }

  private def serverArgs(workingDir: os.Path, explicitSocketOpt: Option[os.Path]): Seq[String] =
    Seq(
      "server",
      // Nothing is going to speak LSP to it: it initializes itself on its working directory, and
      // answers on its command socket alone
      "--lsp=false",
      "--working-dir",
      workingDir.toString
    ) ++
      explicitSocketOpt.toSeq.flatMap(socket => Seq("--socket", socket.toString)) ++
      extraServerArgs()

  /** Extra options for the server we start, from `PLASMON_AUTO_SERVER_ARGS`.
    *
    * The command line has no room for them: the options of an `lsp …` command are the server's own
    * and are parsed there, so an option meant for the server we start would have to be one the
    * server it reaches doesn't know. Whitespace-separated, which rules out values with whitespace
    * in them.
    */
  private def extraServerArgs(): Seq[String] =
    Option(System.getenv("PLASMON_AUTO_SERVER_ARGS"))
      .toSeq
      .flatMap(_.split("\\s+").toSeq)
      .filter(_.nonEmpty)

  /** Whether `socketPath` is the socket of the server we spawned, rather than of one that came up
    * meanwhile.
    *
    * The server names its socket after its own PID (see [[Command.actualSocket]]), which is all we
    * have to go by. Where it doesn't - Windows, or a socket path the caller picked - we take the
    * server as ours.
    */
  private def startedByUs(
    socketPath: os.Path,
    proc: os.SubProcess,
    explicitSocketOpt: Option[os.Path]
  ): Boolean =
    Properties.isWin ||
    explicitSocketOpt.nonEmpty ||
    socketPath.last == proc.wrapped.pid().toString

  private def printOutputTail(output: os.Path, lines: Int = 20): Unit =
    if (os.exists(output)) {
      val content = os.read.lines(output).takeRight(lines)
      if (content.nonEmpty) {
        System.err.println(s"Last ${content.length} lines of $output:")
        for (line <- content)
          System.err.println(s"  $line")
      }
    }

  /** How to start another instance of ourselves, up to the arguments. */
  private def selfCommand(): Option[Seq[String]] =
    BinaryName.pathOpt match {
      case Some(binary) => Some(Seq(binary.toString))
      case None         => javaCommand()
    }

  private def javaCommand(): Option[Seq[String]] = {
    val info = ProcessHandle.current().info()
    val javaBin =
      if (info.command().isPresent) info.command().get()
      else {
        val name = if (Properties.isWin) "java.exe" else "java"
        (os.Path(sys.props("java.home"), os.pwd) / "bin" / name).toString
      }
    val prefixOpt =
      Option.when(info.arguments().isPresent)(info.arguments().get().toVector)
        .flatMap(javaPrefix)
        .orElse(classPathPrefix())
    prefixOpt.map(javaBin +: _)
  }

  /** The `java …` part of our own command line: the JVM options, followed by whatever tells the JVM
    * what to run - `-jar <JAR>`, `-cp <class path> <main class>`, or a main class on its own. What
    * comes after that in the arguments of this process is our own arguments, which the caller
    * replaces with the ones of the server.
    *
    * Taking the options we were started with rather than a bare class path keeps the server we
    * start the same JVM as the one running this: the `--add-opens` it needs to compile Java
    * sources, a `-D` someone set, the heap they picked.
    */
  private def javaPrefix(args: Seq[String]): Option[Seq[String]] = {
    val prefix = new ListBuffer[String]
    var idx    = 0
    var found  = false
    while (!found && idx < args.length) {
      val arg = args(idx)
      if (arg == "-jar" && idx + 1 < args.length) {
        prefix ++= Seq(arg, args(idx + 1))
        idx += 2
        found = true
      }
      else if (
        (arg == "-cp" || arg == "-classpath" || arg == "--class-path") && idx + 2 < args.length
      ) {
        // the class path, then the main class right after it
        prefix ++= Seq(arg, args(idx + 1), args(idx + 2))
        idx += 3
        found = true
      }
      else if (arg.startsWith("-")) {
        // a JVM option - one taking a separate value (-m, -p, …) has it picked up as "main class"
        // below, which puts it in the same place in the command we build anyway
        prefix += arg
        idx += 1
      }
      else {
        // main class, with the class path coming from the environment
        prefix += arg
        idx += 1
        found = true
      }
    }
    Option.when(found)(prefix.toList)
  }

  /** What to run when the arguments of this process are not to be had - Windows, notably, where
    * [[ProcessHandle.Info.arguments]] hands back nothing.
    */
  private def classPathPrefix(): Option[Seq[String]] = {
    val classPath = sys.props.getOrElse("java.class.path", "")
    if (classPath.isEmpty) None
    else if (!classPath.contains(File.pathSeparator) && classPath.endsWith(".jar"))
      // A launcher JAR: let its manifest say what to start, the class path we can see may well be
      // the launcher alone, with the actual one inside it
      Some(Seq("-jar", classPath))
    else
      Some(Seq("-cp", classPath, "plasmon.Plasmon"))
  }
}
