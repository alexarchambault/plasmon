package plasmon.integration

import com.eed3si9n.expecty.Expecty.expect

import java.io.File
import java.net.{ServerSocket, URI}
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.net.URLEncoder
import java.nio.charset.StandardCharsets
import java.util.concurrent.TimeUnit

import scala.util.Properties
import scala.util.control.NonFatal

/** `plasmon browse`, with the plasmon server it runs in its own process, on a Scala CLI project.
  *
  * The server tests (`plasmon.command.BrowseAppTests`) check the web server against fake commands;
  * this checks what the actual commands answer goes through, and that the server `browse` sets up
  * loads what files need on its own.
  */
class BrowseTests extends PlasmonSuite {

  private def jvm          = TestUtil.jvmValues.head
  private def scalaVersion = IntegrationConstants.scala3

  // Undertow and cask aren't set up for native images
  if (TestUtil.launcherKind != "native")
    test("hover, definition and diagnostics") {
      browseTest()
    }

  private def browseTest(): Unit = {
    val fooFile = os.sub / "Foo.scala"
    val barFile = os.sub / "Bar.scala"
    val files = Seq(
      fooFile ->
        s"""//> using scala $scalaVersion
           |//> using jvm ${jvm.value}
           |
           |object Foo {
           |  def greeting: String = "hello"
           |  def main(args: Array[String]): Unit =
           |    println(gre<1>eting)
           |}
           |""".stripMargin,
      barFile ->
        """object Bar {
          |  val n: Int = "not an int"
          |}
          |""".stripMargin
    )

    TestUtil.withWorkspaceNoServer(timeout = Some(TestUtil.baseTimeout * 3))(files*) {
      (workspace, positions) =>

        val tmpDir = workspace / os.up
        // The server finds Scala CLI on the PATH
        val binDir = tmpDir / "bin"
        os.makeDir.all(binDir)
        if (Properties.isWin) os.copy(TestUtil.scalaCli, binDir / "scala-cli.exe")
        else os.symlink(binDir / "scala-cli", TestUtil.scalaCli)

        val command: Seq[os.Shellable] =
          if (TestUtil.launcherKind == "native")
            Seq(TestUtil.launcher, TestUtil.serverExtraJavaOpts)
          else
            Seq(
              "java",
              "--add-opens=java.base/java.util=ALL-UNNAMED",
              IntegrationConstants.javacInternalsJavaOptions,
              TestUtil.serverExtraJavaOpts,
              "-jar",
              TestUtil.launcher
            )
        val port   = freePort()
        val output = tmpDir / "browse-output.txt"
        val proc = os.proc(command, "browse", "--port", port.toString, workspace).spawn(
          cwd = tmpDir,
          env = TestUtil.serverEnv ++ Map(
            "PATH" -> (binDir.toString + File.pathSeparator + sys.env.getOrElse("PATH", ""))
          ),
          stdout = output,
          mergeErrIntoOut = true
        )

        try {
          val http = HttpClient.newHttpClient()
          def get(pathAndQuery: String): HttpResponse[String] =
            http.send(
              HttpRequest.newBuilder(URI.create(s"http://127.0.0.1:$port$pathAndQuery")).build(),
              HttpResponse.BodyHandlers.ofString()
            )
          def json(pathAndQuery: String): ujson.Value = {
            val res = get(pathAndQuery)
            if (res.statusCode() != 200)
              sys.error(s"GET $pathAndQuery: status ${res.statusCode()}: ${res.body()}")
            ujson.read(res.body())
          }
          def enc(s: String) = URLEncoder.encode(s, StandardCharsets.UTF_8)

          // Waiting for it to listen
          var page = Option.empty[HttpResponse[String]]
          while (page.isEmpty) {
            if (!proc.isAlive())
              sys.error(s"plasmon browse exited with code ${proc.exitCode()}")
            page =
              try Some(get("/"))
              catch {
                case NonFatal(_) =>
                  Thread.sleep(200L)
                  None
              }
          }
          expect(page.get.statusCode() == 200)
          expect(page.get.body().contains(s"<title>${workspace.last}</title>"))

          val tree = json("/api/tree?path=").arr.map(_("name").str).toSet
          expect(tree.contains("Foo.scala"))
          expect(tree.contains("Bar.scala"))

          // Loads the Scala CLI project first
          val (line, col) = positions.pos(fooFile, 1)
          val position    = s"path=${enc(fooFile.toString)}&line=$line&col=$col"
          val hover       = json(s"/api/hover?$position")
          expect(hover("markdown").str.contains("def greeting: String"))

          val status = json("/api/status?since=-1")
          expect(status("text").str.startsWith("Foo.scala: ready"))
          expect(!status("error").bool)

          // What the server would tell an editor: its logs, the status of the file
          val channels = status("channels").arr.map(c => c("id").str -> c("label").str).toMap
          expect(channels.get("browse").contains("Browse"))
          expect(channels.get("indexer").contains("Indexer"))
          expect(channels.keys.exists(_.startsWith("bsp-")))
          expect(json("/api/status?since=-1&log=server")("lines").arr.nonEmpty)
          expect(json("/api/status?since=-1&log=indexer")("lines").arr.nonEmpty)
          // Focused like the page does, the server sends status items for it
          json(s"/api/focus?path=${enc(fooFile.toString)}")
          val serverStatus = {
            // The status actor sends updates on its own time
            val deadline = System.currentTimeMillis() + 10000L
            var res      = json("/api/status?since=-1&log=")
            while (
              !res("items").arr.exists(_("id").str == "plasmon.interactive") &&
              System.currentTimeMillis() < deadline
            ) {
              Thread.sleep(100L)
              res = json("/api/status?since=-1&log=")
            }
            res
          }
          expect(serverStatus("file").str == "Foo.scala")
          val interactive = serverStatus("items").arr.find(_("id").str == "plasmon.interactive")
          expect(interactive.exists(_("detail").str.contains(scalaVersion)))

          val locations = json(s"/api/definition?$position")("locations").arr
          expect(locations.length == 1)
          expect(locations(0)("path").str == "Foo.scala")
          expect(locations(0)("line").num == 4) // the greeting definition

          val fooDiagnostics = json(s"/api/diagnostics?path=${enc(fooFile.toString)}")
          expect(fooDiagnostics("diagnostics").arr.isEmpty)

          // In the same module, already loaded
          val barDiagnostics = json(s"/api/diagnostics?path=${enc(barFile.toString)}")
          val diags          = barDiagnostics("diagnostics").arr
          expect(diags.length == 1)
          expect(diags(0)("severity").str == "error")
          expect(diags(0)("line").num == 1)
          expect(diags(0)("message").str.contains("Required: Int"))

          // Its state lives on its own, out of the way of a server an editor would start there
          val stateDir = workspace / ".plasmon" / "browser"
          val socket   = workspace / ".plasmon" / "socket"
          expect(os.isDir(stateDir))
          expect(!os.exists(socket))
        }
        finally {
          proc.destroy()
          if (!proc.waitFor(TimeUnit.SECONDS.toMillis(10L)))
            proc.destroyForcibly()
          for (log <- TestLogs.current)
            if (os.exists(output)) {
              log.println("Output of plasmon browse:")
              log.println(os.read(output))
            }
        }
    }
  }

  private def freePort(): Int = {
    val socket = new ServerSocket(0)
    try socket.getLocalPort
    finally socket.close()
  }
}
