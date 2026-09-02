package plasmon.integration

import com.eed3si9n.expecty.Expecty.expect
import plasmon.integration.TestUtil.*

import scala.util.control.NonFatal

/** `--auto` on the `lsp …` commands: loading whatever a file needs before answering for it.
  *
  * CLI-only by nature. Over LSP there is nothing to test - an editor has loaded a build tool and a
  * module of its own accord long before it asks for a hover, which is exactly the step a terminal
  * has nobody to have taken. So these run through [[ServerDriver.Cli]] alone, and reach into it
  * rather than going through [[ServerDriver]], which has no `--auto` to expose - or, where even a
  * running server is more than the test may assume, run commands with no driver at all.
  *
  * Scala CLI is imported for real here rather than replayed: what is under test is the discovery
  * that picks a build tool out of a workspace, which a recording short-circuits.
  */
class AutoTests extends PlasmonSuite {

  private def jvm          = jvmValues.head
  private def scalaVersion = IntegrationConstants.scala3

  private def cli(driver: ServerDriver): ServerDriver.Cli =
    driver match {
      case cli0: ServerDriver.Cli => cli0
      case other                  => sys.error(s"Expected a CLI driver, got $other")
    }

  for (mode <- cliOnlyModes)
    test("hover and definition load what the file needs" + mode.testNameSuffix) {
      loadsWhatIsNeededTest()
    }

  for (mode <- cliOnlyModes)
    test("hover fails when there is nothing to load" + mode.testNameSuffix) {
      nothingToLoadTest()
    }

  for (mode <- cliOnlyModes)
    test("hover starts a server when none is running" + mode.testNameSuffix) {
      startsServerTest()
    }

  private def loadsWhatIsNeededTest(): Unit = {

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

    withWorkspaceServerPositions(
      mode = TestMode.Cli,
      extraServerOpts = Seq("--jvm", jvm.value),
      timeout = Some(buildTool.defaultTimeout)
    )(files*) {
      (workspace, driver, positions, _) =>

        val driver0 = cli(driver)
        val path    = workspace / sourceFile
        val pos     = positions.lspPos(sourceFile, 1)

        // Nothing has been loaded: no build tool was started, so the file belongs to no build
        // target, and there is no presentation compiler that could answer for it
        expect(driver0.hover(path, pos) == null)

        // --auto discovers Scala CLI, starts it, loads the module the file is in, and only then
        // answers
        val hover = driver0.hoverAuto(path, pos)
        expect(hover != null)
        expect(hover.getContents.getRight.getValue.contains("greeting"))

        // What --auto loaded stays loaded - the plain request now answers too
        val plainHover = driver0.hover(path, pos)
        expect(plainHover != null)
        expect(
          plainHover.getContents.getRight.getValue ==
            hover.getContents.getRight.getValue
        )

        // …which means this exercises the other half of --auto: noticing there is nothing left
        // to load, and not loading a second build tool over the one already there
        val locations = driver0.definitionAuto(path, pos)
        expect(locations.length == 1)
        expect(locations.head.getUri == path.toNIO.toUri.toASCIIString)
        expect(locations.head.getRange.getStart.getLine == 4) // the greeting definition
    }
  }

  private def nothingToLoadTest(): Unit = {

    // No build file, and no using directives that would make this a Scala CLI project either -
    // nothing for discovery to find
    val sourceFile = os.sub / "Foo.scala"

    withWorkspaceServerPositions(
      mode = TestMode.Cli,
      timeout = Some(TestUtil.baseTimeout)
    )(
      sourceFile ->
        """object Foo {
          |  def greeting: String = "hello"
          |  def main(args: Array[String]): Unit =
          |    println(gre<1>eting)
          |}
          |""".stripMargin
    ) {
      (workspace, driver, positions, _) =>

        val driver0 = cli(driver)
        val path    = workspace / sourceFile
        val pos     = positions.lspPos(sourceFile, 1)

        val ex = intercept[Exception](driver0.hoverAuto(path, pos))
        expect(ex.getMessage.contains("exit code 1"))

        // Without --auto the very same request is not an error, it just has nothing to say
        expect(driver0.hover(path, pos) == null)
    }
  }

  /** `--auto` with nothing running at all - not even a server.
    *
    * The one thing every other test here is handed is the thing this one must not have, hence
    * [[TestUtil.withWorkspaceNoServer]] and raw commands rather than a [[ServerDriver]]: a driver
    * is a server that someone already started.
    */
  private def startsServerTest(): Unit = {

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

    // What the harness gives the servers it starts itself - the Scala CLI to import with, and the
    // JVM to run it on. A server started behind our back gets them from here.
    val autoServerArgs = Map(
      "PLASMON_AUTO_SERVER_ARGS" ->
        s"--scala-cli ${TestUtil.scalaCli} --jvm ${jvm.value}"
    )

    withWorkspaceNoServer(timeout = Some(buildTool.defaultTimeout))(files*) {
      (workspace, positions) =>

        val path        = workspace / sourceFile
        val (line, col) = positions.pos(sourceFile, 1)

        // Where a server writes down the socket it can be reached on
        val socketFile = workspace / ".plasmon" / "socket"

        def request(command: String, extraArgs: String*): String =
          TestUtil.serverCommandOutput(workspace, TestLogs.currentStream, autoServerArgs)(
            "lsp",
            command,
            "--line",
            line.toString,
            "--col",
            col.toString,
            extraArgs,
            path
          )

        def hover(extraArgs: String*): String =
          request("hover", extraArgs*)

        def serverAnswers(): Boolean =
          try {
            TestUtil.runServerCommand(workspace, TestLogs.currentStream)("about")
            true
          }
          catch {
            case NonFatal(_) => false
          }

        /** Stops the server, and waits for it to be gone - `exit` schedules the shutdown rather
          * than waiting for it, so the socket answers for a moment longer.
          */
        def stopServer(): Unit = {
          TestUtil.runServerCommand(workspace, TestLogs.currentStream)("exit")
          val deadline = System.currentTimeMillis() + TestUtil.baseTimeout.toMillis
          while (serverAnswers())
            if (System.currentTimeMillis() >= deadline)
              sys.error(s"Server in $workspace still answering after ${TestUtil.baseTimeout}")
            else
              Thread.sleep(500L)
        }

        // Nobody started a server here, so nothing wrote down where one could be reached
        expect(!os.exists(socketFile))

        // …which used to be as far as this got. --auto starts one, and answers through it
        val hovered = hover("--auto")
        expect(hovered.contains("greeting"))

        // The server it started stays up, with what --auto loaded in it still loaded: the same
        // request, this time with nothing to start and nothing to load, answers the same
        expect(os.exists(socketFile))
        expect(hover().contains("greeting"))

        // Every request that answers for a file takes --auto, and finding nothing left to start
        // or load is as much a part of it as doing the starting
        expect(request("completion", "--auto").contains("greeting"))

        // Second time round, in a workspace that now has state persisted in it. The server --auto
        // starts restores that in the background, while already answering commands, so --auto has
        // to wait for it - otherwise it sees no build tool for the file and tries to load one over
        // the one on its way back
        stopServer()
        expect(hover("--auto").contains("greeting"))
    }
  }
}
