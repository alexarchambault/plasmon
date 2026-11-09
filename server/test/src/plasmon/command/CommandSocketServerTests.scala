package plasmon.command

import caseapp.core.RemainingArgs
import com.github.plokhotnyuk.jsoniter_scala.core.writeToString
import com.google.gson.JsonParser
import org.eclipse.lsp4j.jsonrpc.Launcher
import plasmon.protocol.{CommandClient, CommandServer, PrintData, RemainingArgsJson}
import plasmon.protocol as p
import plasmon.servercommand.{AboutOptions, ServerCommandInstance}
import plasmon.util.ThreadUtil

import java.net.{StandardProtocolFamily, UnixDomainSocketAddress}
import java.nio.channels.{ServerSocketChannel, SocketChannel}
import java.nio.file.Files
import java.util.concurrent.{
  CompletableFuture,
  ConcurrentLinkedQueue,
  CountDownLatch,
  Executors,
  TimeUnit
}
import java.util.concurrent.atomic.AtomicReference

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.DurationInt
import scala.jdk.CollectionConverters.*

/** Several `plasmon …` invocations talking to one server at once */
class CommandSocketServerTests extends munit.FunSuite {

  /** A command printing its argument a few times, giving other commands time to run meanwhile */
  private final class Echo(client: CommandClient)
      extends ServerCommandInstance[AboutOptions](client) {
    override def names = List(List("echo"))
    def run(options: AboutOptions, args: RemainingArgs): Unit =
      for (i <- 1 to 5) {
        printLine(s"${args.all.mkString(" ")} $i")
        Thread.sleep(20L)
      }
  }

  /** A command printing until printing fails */
  private final class Spin(
    client: CommandClient,
    failure: AtomicReference[Throwable] = new AtomicReference,
    done: CountDownLatch = new CountDownLatch(1)
  ) extends ServerCommandInstance[AboutOptions](client) {
    override def names = List(List("spin"))
    def run(options: AboutOptions, args: RemainingArgs): Unit =
      try
        while (true) {
          printLine("spinning")
          Thread.sleep(20L)
        }
      catch {
        case t: Throwable =>
          failure.set(t)
          throw t
      }
      finally done.countDown()
  }

  private def withServer[T](
    commands: CommandClient => Seq[ServerCommandInstance[?]]
  )(f: UnixDomainSocketAddress => T): T = {
    val dir     = Files.createTempDirectory("plasmon-command-socket")
    val addr    = UnixDomainSocketAddress.of(dir.resolve("socket"))
    val channel = ServerSocketChannel.open(StandardProtocolFamily.UNIX)
    channel.bind(addr)
    val thread = new Thread("test-command-server") {
      setDaemon(true)
      override def run(): Unit =
        try CommandSocketServer.run(channel, commands, logJsonrpc = false)
        catch {
          case _: java.nio.channels.AsynchronousCloseException =>
        }
    }
    thread.start()
    try f(addr)
    finally {
      channel.close()
      thread.join(10000L)
      Files.deleteIfExists(addr.getPath)
      Files.deleteIfExists(dir)
    }
  }

  private def command(name: String, args: String*): p.Command = {
    val cmd = new p.Command
    cmd.setName(Array(name))
    cmd.setRemainingArgs(
      JsonParser.parseString(
        writeToString(
          RemainingArgsJson(args.toList.zipWithIndex.map { case (a, i) =>
            RemainingArgsJson.Arg(i, 1, a)
          })
        )
      )
    )
    cmd
  }

  // An anonymous class rather than a lambda: lsp4j looks for the annotated methods on its class
  private def commandClient(f: PrintData => CompletableFuture[Unit]): CommandClient =
    new CommandClient {
      def print(data: PrintData): CompletableFuture[Unit] = f(data)
    }

  /** A client collecting what it's sent */
  private def collectingClient(): (CommandClient, () => List[String]) = {
    val lines = new ConcurrentLinkedQueue[String]
    val client = commandClient { data =>
      lines.add(data.getLine)
      CompletableFuture.completedFuture(())
    }
    (client, () => lines.asScala.toList)
  }

  /** A client going away on the first print, without answering it - like a `plasmon` process
    * exiting (on Ctrl+C, say) while the server waits on a print of its
    */
  private def goingAwayClient(channel: SocketChannel): CommandClient =
    commandClient { _ =>
      channel.close()
      new CompletableFuture[Unit]
    }

  /** Connects to the server like the CLI does, and runs a command from there */
  private def connect(
    addr: UnixDomainSocketAddress,
    client: SocketChannel => CommandClient
  ): (SocketChannel, CommandServer) = {
    val channel = SocketChannel.open(StandardProtocolFamily.UNIX)
    channel.connect(addr)
    val socket = libdaemonjvm.Util.socketFromChannel(channel)
    val launcher = new Launcher.Builder[CommandServer]()
      .setExecutorService(Executors.newCachedThreadPool(
        ThreadUtil.daemonThreadFactory("test-client")
      ))
      .setInput(socket.getInputStream)
      .setOutput(socket.getOutputStream)
      .setRemoteInterface(classOf[CommandServer])
      .setLocalService(client(channel))
      .create()
    launcher.startListening()
    (channel, launcher.getRemoteProxy)
  }

  test("concurrent commands each print to their own client") {
    withServer(client => Seq(new Echo(client))) { addr =>
      val count = 8
      val pool  = Executors.newFixedThreadPool(count, ThreadUtil.daemonThreadFactory("test"))
      implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(pool)
      try {
        val runs = (1 to count).map { id =>
          Future {
            val (client, lines)   = collectingClient()
            val (channel, server) = connect(addr, _ => client)
            try {
              val res = server.runCommand(command("echo", s"client-$id")).get(20L, TimeUnit.SECONDS)
              id -> (res.getExitCode, lines())
            }
            finally channel.close()
          }
        }
        val results = Await.result(Future.sequence(runs), 1.minute).toMap
        for (id <- 1 to count) {
          val (exitCode, lines) = results(id)
          assertEquals(exitCode, 0)
          assertEquals(lines, (1 to 5).map(i => s"client-$id $i").toList)
        }
      }
      finally pool.shutdown()
    }
  }

  test("a command whose client went away fails rather than hangs") {
    val failure = new AtomicReference[Throwable]
    val done    = new CountDownLatch(1)
    withServer(client => Seq(new Spin(client, failure, done))) { addr =>
      val (_, server) = connect(addr, goingAwayClient)
      server.runCommand(command("spin"))
      assert(done.await(20L, TimeUnit.SECONDS), "Command still running after its client went away")
      assert(
        failure.get().isInstanceOf[ConnectionCommandClient.ClientGoneException],
        s"Unexpected failure: ${failure.get()}"
      )
    }
  }

  test("commands keep running after many clients went away") {
    // Before each connection had its own client, each of these could hold one of a fixed number of
    // command threads forever
    val count = 10
    val done  = new CountDownLatch(count)
    withServer(client => Seq(new Spin(client, done = done), new Echo(client))) { addr =>
      for (_ <- 1 to count) {
        val (_, server) = connect(addr, goingAwayClient)
        server.runCommand(command("spin"))
      }

      val (client, lines)   = collectingClient()
      val (channel, server) = connect(addr, _ => client)
      try {
        val res = server.runCommand(command("echo", "still")).get(20L, TimeUnit.SECONDS)
        assertEquals(res.getExitCode, 0)
        assertEquals(lines(), (1 to 5).map(i => s"still $i").toList)
      }
      finally channel.close()

      assert(
        done.await(20L, TimeUnit.SECONDS),
        s"${done.getCount} command(s) still running after their client went away"
      )
    }
  }
}
