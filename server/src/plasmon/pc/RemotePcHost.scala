package plasmon.pc

import org.eclipse.lsp4j as l
import org.eclipse.lsp4j.jsonrpc.{Launcher, ResponseErrorException}
import plasmon.internal.{BuildVersion, Constants}
import plasmon.remotepc.{Messages, RemotePcClient, RemotePcServer}

import java.io.{BufferedReader, File, InputStreamReader}
import java.nio.charset.StandardCharsets
import java.util.{List as JList, Optional}
import java.util.concurrent.{
  CompletableFuture,
  CompletionException,
  ConcurrentHashMap,
  ExecutionException,
  ExecutorService,
  Executors,
  TimeUnit
}
import java.util.concurrent.atomic.AtomicInteger

import scala.jdk.CollectionConverters.*
import scala.meta.internal.mtags.SourcePath
import scala.meta.pc.{
  ContentType,
  DisplayableException,
  MemberKind,
  SourcePathContext,
  SymbolSearchVisitor
}
import scala.util.control.NonFatal

/** A process running presentation compilers for us, that we talk to with the JSON-RPC protocol of
  * `plasmon.remotepc`, over its standard input and output
  *
  * The process is started when a presentation compiler is first needed, and started again if it
  * exits. It exits by itself once we're gone, when its standard input gets closed.
  *
  * @param name
  *   name of the host, for logging
  * @param scalaVersion
  *   version of the presentation compilers of the host
  * @param classPath
  *   class path of the host, computed when it's first needed
  * @param mainClass
  *   main class of the host
  */
final class RemotePcHost(
  name: String,
  val scalaVersion: String,
  classPath: () => Seq[os.Path],
  mainClass: String,
  javaHome: os.Path
) extends AutoCloseable {

  import RemotePcHost.*

  private val pool: ExecutorService =
    Executors.newCachedThreadPool(daemonThreadFactory(s"$name-pc-host"))

  /** Presentation compilers we created, that the host may call back */
  private val instances = new ConcurrentHashMap[String, RemotePresentationCompiler]

  private[pc] def register(pc: RemotePresentationCompiler): Unit =
    instances.put(pc.pcId, pc)
  private[pc] def unregister(pc: RemotePresentationCompiler): Unit =
    instances.remove(pc.pcId, pc)

  /** Whether we can start the host, that is, whether we could get its class path */
  lazy val classPathOrError: Either[Throwable, Seq[os.Path]] =
    try Right(classPath())
    catch {
      case NonFatal(e) =>
        scribe.warn(s"Could not get the class path of the $name presentation compiler host", e)
        Left(e)
    }

  /** A started host process
    *
    * The protocol doesn't fail the requests that are pending when the other side goes away, so we
    * keep track of them ourselves, to fail them if the host exits.
    */
  private[pc] final class Connection(
    val id: Int,
    process: Process,
    remote: RemotePcServer
  ) {
    @volatile var isAlive = true

    /** Completed once we're done handling the exit of the host */
    val exitHandled     = new CompletableFuture[Unit]
    private val pending = ConcurrentHashMap.newKeySet[CompletableFuture[?]]()

    /** Presentation compilers created on this host, and the outcome of their creation */
    val created = new ConcurrentHashMap[String, CompletableFuture[Messages.CreateResult]]

    def call[T](f: RemotePcServer => CompletableFuture[T]): CompletableFuture[T] =
      if (isAlive) {
        val future =
          try f(remote)
          catch {
            // sending fails if the host exited and we weren't notified yet
            case NonFatal(e) =>
              CompletableFuture.failedFuture(new HostExitedException(name, e))
          }
        pending.add(future)
        future.whenComplete((_, _) => pending.remove(future))
        // the host might have exited while we were adding it
        if (!isAlive)
          future.completeExceptionally(new HostExitedException(name))
        future
      }
      else
        CompletableFuture.failedFuture(new HostExitedException(name))

    def notify(f: RemotePcServer => Unit): Unit =
      if (isAlive)
        try f(remote)
        catch {
          case NonFatal(e) =>
            scribe.warn(s"Error sending notification to the $name presentation compiler host", e)
        }

    def onExit(): Unit = {
      isAlive = false
      for (future <- pending.asScala.toVector)
        future.completeExceptionally(new HostExitedException(name))
    }

    /** Asks the host to exit, and waits for it to do so
      *
      * Things like shutdown hooks of the host run when it exits, so we give it some time before
      * killing it.
      */
    def destroy(): Unit = {
      notify(_.exit())
      try process.getOutputStream.close()
      catch {
        case NonFatal(_) =>
      }
      if (!process.waitFor(gracefulExitTimeoutSeconds, TimeUnit.SECONDS)) {
        scribe.warn(s"$name presentation compiler host #$id didn't exit, killing it")
        process.destroy()
        if (!process.waitFor(5L, TimeUnit.SECONDS))
          process.destroyForcibly()
      }
      try exitHandled.get(10L, TimeUnit.SECONDS)
      catch {
        case NonFatal(e) =>
          scribe.warn(s"Error waiting for $name presentation compiler host #$id to be done", e)
      }
    }
  }

  private var connectionOpt = Option.empty[Connection]
  private var closed        = false
  private val connectionIds = new AtomicInteger
  private[pc] def connection(): Connection =
    synchronized {
      if (closed)
        throw new IllegalStateException(s"The $name presentation compiler host was closed")
      connectionOpt.filter(_.isAlive).getOrElse {
        val conn = start()
        connectionOpt = Some(conn)
        conn
      }
    }

  private lazy val javaVersion = plasmon.bsp.BspDataPortability.majorFromJdkFiles(javaHome)

  private def start(): Connection = {
    val cp = classPathOrError.fold(e => throw e, identity)
    val id = connectionIds.incrementAndGet()
    val loggingOptions =
      // The JVM logs warnings to its standard output, which is our JSON-RPC channel
      if (javaVersion.exists(_ >= 9))
        Seq("-Xlog:disable", "-Xlog:all=warning:stderr")
      else Nil
    val command = Seq(
      (javaHome / "bin" / (if (scala.util.Properties.isWin) "java.exe" else "java")).toString,
      // like Metals does for its presentation compilers, that can recurse deeply
      "-Xss4m"
    ) ++ loggingOptions ++ Seq(
      "-cp",
      cp.map(_.toString).mkString(File.pathSeparator),
      mainClass
    )
    scribe.info(s"Starting $name presentation compiler host #$id")
    scribe.debug(s"$name presentation compiler host #$id command: ${command.mkString(" ")}")
    val process = new ProcessBuilder(command*)
      .redirectError(ProcessBuilder.Redirect.PIPE)
      .start()

    val stderrThread = new Thread(s"$name-pc-host-$id-stderr") {
      setDaemon(true)
      override def run(): Unit = {
        val reader = new BufferedReader(
          new InputStreamReader(process.getErrorStream, StandardCharsets.UTF_8)
        )
        try {
          var line = reader.readLine()
          while (line != null) {
            scribe.info(s"[$name PC host #$id] $line")
            line = reader.readLine()
          }
        }
        catch {
          case NonFatal(_) =>
        }
      }
    }
    stderrThread.start()

    var conn: Connection = null
    val client           = new Client(() => conn)
    val launcher = new Launcher.Builder[RemotePcServer]()
      .setLocalService(client)
      .setRemoteInterface(classOf[RemotePcServer])
      .setInput(process.getInputStream)
      .setOutput(process.getOutputStream)
      .setExecutorService(pool)
      .create()
    conn = new Connection(id, process, launcher.getRemoteProxy)
    launcher.startListening()

    process.onExit().thenRun { () =>
      val exitCode = process.exitValue()
      if (exitCode == 0) scribe.info(s"$name presentation compiler host #$id exited")
      else scribe.warn(s"$name presentation compiler host #$id exited with code $exitCode")
      try {
        conn.onExit()
        for (pc <- instances.values().asScala)
          pc.hostExited(conn)
      }
      finally conn.exitHandled.complete(())
    }

    conn
  }

  def isStarted: Boolean =
    synchronized(connectionOpt.exists(_.isAlive))

  override def close(): Unit = {
    val connOpt = synchronized {
      closed = true
      val former = connectionOpt
      connectionOpt = None
      former
    }
    for (conn <- connOpt)
      conn.destroy()
    pool.shutdown()
  }

  /** Answers the requests of the host, for the presentation compilers it runs */
  private final class Client(connection: () => Connection) extends RemotePcClient {

    private def instance(pcId: String): Option[RemotePresentationCompiler] =
      Option(instances.get(pcId))

    private def async[T](pcId: String)(
      f: RemotePresentationCompiler => T
    ): CompletableFuture[T] =
      instance(pcId) match {
        case Some(pc) =>
          // These can call the host back, so they mustn't run on the thread reading its messages
          CompletableFuture.supplyAsync(() => f(pc), pool)
        case None =>
          CompletableFuture.failedFuture(
            new IllegalArgumentException(s"No presentation compiler with id $pcId")
          )
      }

    def log(params: Messages.LogParams): Unit =
      instance(params.pcId).foreach(_.log(params.message))

    def accessStarted(params: Messages.AccessParams): Unit =
      instance(params.pcId).foreach(_.accessStarted(params.requestId, params.name, params.uri))

    def accessEnded(params: Messages.AccessParams): Unit =
      instance(params.pcId).foreach(_.accessEnded(params.requestId, params.name, params.uri))

    def documentation(params: Messages.SearchParams)
      : CompletableFuture[Messages.SymbolDocumentationWire] =
      async(params.pcId) { pc =>
        pc.search
          .documentation(
            params.module,
            params.symbol,
            params.parentSymbols(),
            Option(params.contentType).map(ContentType.valueOf).getOrElse(ContentType.MARKDOWN),
            pc.userLogger
          )
          .map(Messages.SymbolDocumentationWire.from)
          .orElse(null)
      }

    def definition(params: Messages.SearchParams): CompletableFuture[JList[l.Location]] =
      async(params.pcId) { pc =>
        pc.search.definition(
          params.module,
          params.symbol,
          Option(params.uri).map(java.net.URI.create).orNull
        )
      }

    def definitionSourceToplevels(params: Messages.SearchParams)
      : CompletableFuture[JList[String]] =
      async(params.pcId) { pc =>
        pc.search.definitionSourceToplevels(
          params.module,
          params.symbol,
          Option(params.uri).map(java.net.URI.create).orNull
        )
      }

    def search(params: Messages.SearchParams): CompletableFuture[String] =
      async(params.pcId) { pc =>
        val visitor = new RemoteVisitor(connection(), params.searchId)
        val kind: Optional[MemberKind] =
          Optional.ofNullable(params.kind).map(MemberKind.valueOf)
        SourcePath.withContext { ctx =>
          pc.search.search(params.query, params.module, kind, visitor, ctx.iface).name
        }
      }

    def searchMethods(params: Messages.SearchParams): CompletableFuture[String] =
      async(params.pcId) { pc =>
        val visitor = new RemoteVisitor(connection(), params.searchId)
        pc.search.searchMethods(params.query, params.module, visitor).name
      }

    def workspaceMemberPriority(params: Messages.SearchParams): CompletableFuture[Integer] =
      async(params.pcId) { pc =>
        pc.completionItemPriority.workspaceMemberPriority(params.symbol)
      }
  }

  /** Sends what's visited back to the host, where the actual visitor of the search runs
    *
    * We don't know which packages the actual visitor wants to visit, so we accept them all here,
    * and let the host check them when visiting their class files.
    */
  private final class RemoteVisitor(conn: Connection, searchId: String)
      extends SymbolSearchVisitor {
    @volatile private var cancelled = false

    private def visit(fill: Messages.VisitParams => Unit): Int =
      if (cancelled) 0
      else {
        val params = new Messages.VisitParams
        params.searchId = searchId
        fill(params)
        try {
          val res = conn.call(_.searchVisit(params)).get()
          cancelled = res.cancelled
          res.added
        }
        catch {
          case e: ExecutionException =>
            scribe.warn(s"Error visiting search candidate on $name presentation compiler host", e)
            cancelled = true
            0
        }
      }

    def shouldVisitPackage(pkg: String): Boolean = true

    def visitClassfile(pkg: String, filename: String, ctx: SourcePathContext): Int =
      visit { params =>
        params.kind = Messages.VisitParams.CLASSFILE
        params.pkg = pkg
        params.filename = filename
      }

    def visitWorkspaceSymbol(
      path: java.nio.file.Path,
      symbol: String,
      kind: l.SymbolKind,
      range: l.Range
    ): Int =
      visit { params =>
        params.kind = Messages.VisitParams.WORKSPACE_SYMBOL
        params.path = path.toString
        params.symbol = symbol
        params.symbolKind = kind.getValue
        params.range = range
      }

    override def visitWorkspacePackage(pkg: String): Int =
      visit { params =>
        params.kind = Messages.VisitParams.WORKSPACE_PACKAGE
        params.pkg = pkg
      }

    def isCancelled: Boolean = cancelled
  }
}

object RemotePcHost {

  final class HostExitedException(name: String, cause: Throwable = null)
      extends Exception(s"The $name presentation compiler host exited", cause)

  /** How long we let hosts exit by themselves */
  private def gracefulExitTimeoutSeconds = 30L

  private def daemonThreadFactory(prefix: String): java.util.concurrent.ThreadFactory = {
    val count = new AtomicInteger
    (r: Runnable) => {
      val t = new Thread(r, s"$prefix-${count.incrementAndGet()}")
      t.setDaemon(true)
      t
    }
  }

  /** Turns what the host answered us with back into what presentation compilers throw */
  def unwrapError(t: Throwable): Throwable =
    t match {
      case e: CompletionException if e.getCause != null => unwrapError(e.getCause)
      case e: ExecutionException if e.getCause != null  => unwrapError(e.getCause)
      case e: ResponseErrorException
          if e.getResponseError.getCode == Messages.DISPLAYABLE_ERROR_CODE =>
        new DisplayableException(e.getResponseError.getMessage)
      case other => other
    }

  def scalaBinaryVersion(scalaVersion: String): String =
    scalaVersion.split('.').take(2).mkString(".")

  /** Scala 2 presentation compiler hosts we have, as Scala binary version -> Scala version */
  def scala2Versions: Map[String, String] = Constants.scala2PcVersions

  /** Name of the Java property and environment variable that can force the class path of a Scala 2
    * presentation compiler host, like `plasmon.scala213PcHost.classPath` and
    * `PLASMON_SCALA213_PC_HOST_CLASS_PATH`
    *
    * Mostly meant for development, where the hosts aren't published.
    */
  def scala2ClassPathProperty(scalaBinaryVersion: String) =
    s"plasmon.scala${scalaBinaryVersion.filter(_ != '.')}PcHost.classPath"
  def scala2ClassPathEnvVar(scalaBinaryVersion: String) =
    s"PLASMON_SCALA${scalaBinaryVersion.filter(_ != '.')}_PC_HOST_CLASS_PATH"

  def scala2MainClass = "plasmon.remotepc.host.Scala2PcHost"

  /** The Scala 2 presentation compiler hosts, per Scala binary version
    *
    * Creating them is cheap, they only fetch and start something when they're first needed.
    */
  def scala2Hosts(javaHome: os.Path): Map[String, RemotePcHost] =
    scala2Versions.map {
      case (sbv, sv) =>
        sbv -> new RemotePcHost(
          s"Scala $sbv",
          sv,
          () => scala2ClassPath(sbv),
          scala2MainClass,
          javaHome
        )
    }

  private def snapshotsRepository =
    "https://central.sonatype.com/repository/maven-snapshots"

  /** Class path of the Scala 2 presentation compiler host of a Scala binary version, fetched with
    * coursier unless forced via [[scala2ClassPathProperty]] or [[scala2ClassPathEnvVar]]
    */
  def scala2ClassPath(scalaBinaryVersion: String): Seq[os.Path] = {
    val forced = sys.props.get(scala2ClassPathProperty(scalaBinaryVersion))
      .orElse(sys.env.get(scala2ClassPathEnvVar(scalaBinaryVersion)))
      .map(_.trim)
      .filter(_.nonEmpty)
    forced match {
      case Some(cp) =>
        cp.split(File.pathSeparator).toSeq.filter(_.nonEmpty).map(os.Path(_, os.pwd))
      case None =>
        val version = BuildVersion.version
        if (version == "unknown")
          sys.error(
            s"Unknown Plasmon version, cannot fetch the Scala $scalaBinaryVersion presentation " +
              s"compiler host (set ${scala2ClassPathEnvVar(scalaBinaryVersion)} to its class path)"
          )
        val moduleName = Constants.scala2PcHostModuleNames.getOrElse(
          scalaBinaryVersion,
          sys.error(s"No presentation compiler host for Scala $scalaBinaryVersion")
        )
        val dependency = coursierapi.Dependency.of(Constants.organization, moduleName, version)
        scribe.info(
          s"Fetching the Scala $scalaBinaryVersion presentation compiler host ($dependency)"
        )
        val fetch = coursierapi.Fetch.create().addDependencies(dependency)
        if (!BuildVersion.isStable)
          fetch.addRepositories(coursierapi.MavenRepository.of(snapshotsRepository))
        fetch.fetch().asScala.toSeq.map(f => os.Path(f, os.pwd))
    }
  }
}
