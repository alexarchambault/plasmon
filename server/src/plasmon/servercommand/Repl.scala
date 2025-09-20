package plasmon.servercommand

// import ammonite.sshd.*
import caseapp.core.RemainingArgs
import plasmon.protocol.CommandClient
import plasmon.index.Indexer
import plasmon.Server
// import java.net.ServerSocket
// import org.apache.sshd.server.auth.password.PasswordAuthenticator
// import org.apache.sshd.server.session.ServerSession

final case class Repl(
  server: Server,
  client: CommandClient
) extends ServerCommandInstance[ReplOptions](client) {

  // private def randomPort() = {
  //   val socket = new ServerSocket(0)
  //   try socket.getLocalPort
  //   finally socket.close()
  // }

  def defaultPredef =
    """import plasmon.ide.HoverExtParams
      |import scala.meta.pc.CancelToken
      |import plasmon.ide.FutureCancelToken
      |import scala.concurrent.Promise
      |import org.eclipse.{lsp4j => l}
      |import java.nio.file.Paths
      |import scala.concurrent._
      |import scala.concurrent.duration._
      |import scala.jdk.CollectionConverters._
      |
      |def dummyToken: CancelToken = {
      |  import scala.concurrent.ExecutionContext.Implicits.global
      |  FutureCancelToken(Promise[Boolean]().future)
      |}
      |""".stripMargin

  def run(options: ReplOptions, args: RemainingArgs): Unit = {
    printLine("Repl command disabled in native mode")
    // val port = randomPort()
    // val replServer = new SshdRepl(
    //   SshServerConfig(
    //     address = "localhost",
    //     port = port,
    //     passwordAuthenticator = Some {
    //       new PasswordAuthenticator {
    //         override def authenticate(username: String, password: String, session: ServerSession): Boolean =
    //           username == "repl" && password == "foo"
    //       }
    //     }
    //   ),
    //   replArgs = Seq(
    //     "server" -> server,
    //     "client" -> client
    //   )
    // )
    // client.print(new PrintData(s"Starting server on localhost:$port", true))
    // replServer.start()
    // client.print(new PrintData("Connect to it with:", true))
    // client.print(new PrintData(s"    ssh repl@localhost -p $port", true))
  }
}

object Repl extends ServerCommand[ReplOptions] {
  def instance(
    server: Server,
    indexer: Indexer,
    client: CommandClient,
    lspServer: plasmon.jsonrpc.JsonrpcServer,
    pool: plasmon.command.ServerCommandThreadPools
  ): ServerCommandInstance[ReplOptions] =
    Repl(server, client)
}
