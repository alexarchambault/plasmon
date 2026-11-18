package plasmon.integration

import com.eed3si9n.expecty.Expecty.expect
import plasmon.integration.TestUtil.*

import scala.util.Using

/** `server --bsp <socket>`: a BSP server someone else started, added as a build tool on start-up.
  *
  * Scala CLI stands in for that BSP server, run behind a [[BspSocketBridge]] - nothing loads it as
  * a build tool of its own here, the server only ever sees the socket.
  */
class BspSocketTests extends PlasmonSuite {

  private def jvm          = jvmValues.head
  private def scalaVersion = IntegrationConstants.scala3

  for (mode <- modes)
    test("build server listening on a socket" + mode.testNameSuffix) {
      bspSocketTest(mode)
    }

  private def bspSocketTest(mode: TestMode): Unit = {

    val buildTool = SingleModuleBuildTool.ScalaCli()

    val (sourceFile, files) = buildTool.singleFile(
      os.sub / "Foo.scala",
      s"""//> using scala $scalaVersion
         |//> using jvm ${jvm.value}
         |
         |object Foo {
         |  def greeting: String = "hello"
         |  def main(args: Array[String]): Unit =
         |    println(gre<1>eting)
         |}
         |""".stripMargin
    )

    // Away from the workspace, whose path can be too long for a UNIX domain socket
    val socketDir = os.temp.dir(prefix = "plasmon-bsp-")
    val socket    = socketDir / "bsp"

    try
      Using.resource(
        new BspSocketBridge(
          socket,
          workspace => Seq[os.Shellable](scalaCli, "bsp", "--", workspace),
          None,
          buildTool.defaultTimeout
        )
      ) { bridge =>
        withWorkspaceServerPositions(
          mode = mode,
          extraServerOpts = Seq("--jvm", jvm.value, "--bsp", socket.toString),
          timeout = Some(buildTool.defaultTimeout),
          beforeServer = bridge.setWorkspace
        )(files*) {
          (workspace, driver, positions, osOpt) =>

            // The server connects on its own, but in the background - wait for it
            val deadline = System.currentTimeMillis() + buildTool.defaultTimeout.toMillis
            def bspList(): String =
              serverCommandOutput(workspace, osOpt)("bsp", "list")
            var list = bspList()
            while (!list.contains(socket.toString) && System.currentTimeMillis() < deadline) {
              Thread.sleep(200L)
              list = bspList()
            }
            expect(list.contains(socket.toString))
            expect(bridge.connectionCount == 1)

            driver.loadAllModules(toplevelCacheOnly = false)

            val path  = workspace / sourceFile
            val pos   = positions.lspPos(sourceFile, 1)
            val hover = driver.hover(path, pos)
            expect(hover != null)
            expect(hover.getContents.getRight.getValue.contains("greeting"))

            // What was passed on the command line is for this run of the server alone
            val persisted = workspace / ".plasmon/build-servers.json"
            if (os.exists(persisted))
              expect(!os.read(persisted).contains(socket.toString))
        }
      }
    finally
      os.remove.all(socketDir)
  }
}
