package plasmon.integration

import com.eed3si9n.expecty.Expecty.expect
import com.google.gson.Gson
import org.eclipse.lsp4j as l
import plasmon.integration.TestUtil.*

import scala.concurrent.duration.{DurationInt, FiniteDuration}

/** Automatic mode, in editors: a build tool and a module get loaded for the files users open,
  * rather than picked by hand from a menu.
  *
  * LSP-only: switching modes and focusing files is what editors do. The command line has `--auto`
  * on its `lsp …` commands instead, see [[AutoTests]].
  *
  * Scala CLI is imported for real here rather than replayed, as in [[AutoTests]]: what is under
  * test is the discovery that picks a build tool out of a workspace, which a recording
  * short-circuits.
  */
class AutoModeTests extends PlasmonSuite {

  private def jvm          = jvmValues.head
  private def scalaVersion = IntegrationConstants.scala3

  private def lsp(driver: ServerDriver): ServerDriver.Lsp =
    driver match {
      case lsp0: ServerDriver.Lsp => lsp0
      case other                  => sys.error(s"Expected an LSP driver, got $other")
    }

  private def modeOf(res: Object): String =
    new Gson().toJsonTree(res).getAsJsonObject.get("mode").getAsString

  private def persistedMode(workspace: os.Path): String =
    ujson.read(os.read(workspace / ".plasmon/mode.json"))("mode").str

  /** Hovers until something comes back - a module being loaded in the background, there is no
    * request to wait on.
    */
  private def awaitHover(
    driver: ServerDriver,
    path: os.Path,
    pos: l.Position,
    timeout: FiniteDuration
  ): l.Hover = {
    val deadline = System.currentTimeMillis() + timeout.toMillis
    var res      = driver.hover(path, pos)
    while (res == null && System.currentTimeMillis() < deadline) {
      Thread.sleep(500L)
      res = driver.hover(path, pos)
    }
    if (res == null)
      sys.error(s"No hover for $path at $pos after $timeout")
    res
  }

  private def project() =
    SingleModuleBuildTool.ScalaCli().singleFile(
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

  for (mode <- lspOnlyModes)
    test("switching to automatic mode loads what the focused file needs" + mode.testNameSuffix) {
      switchingTest()
    }

  for (mode <- lspOnlyModes)
    test("--auto starts the server in automatic mode" + mode.testNameSuffix) {
      startInAutoModeTest()
    }

  private def switchingTest(): Unit = {

    val (sourceFile, files) = project()

    withWorkspaceServerPositions(
      mode = TestMode.Lsp,
      extraServerOpts = Seq("--jvm", jvm.value),
      timeout = Some(SingleModuleBuildTool.ScalaCli().defaultTimeout)
    )(files*) {
      (workspace, driver, positions, _) =>

        val driver0 = lsp(driver)
        val path    = workspace / sourceFile
        val pos     = positions.lspPos(sourceFile, 1)

        // Manual mode by default: opening a file loads nothing
        expect(modeOf(driver0.executeCommand("plasmon/mode")) == "manual")
        driver0.didOpen(path, 0, os.read(path))
        expect(driver0.hover(path, pos) == null)

        // Switching to automatic mode loads a build tool and a module for the focused file
        expect(modeOf(driver0.executeCommand("plasmon/setMode", "automatic")) == "automatic")
        expect(persistedMode(workspace) == "automatic")
        val hover = awaitHover(driver0, path, pos, 2.minutes)
        expect(hover.getContents.getRight.getValue.contains("greeting"))

        // Back to manual mode: what was loaded stays loaded
        expect(modeOf(driver0.executeCommand("plasmon/setMode", "manual")) == "manual")
        expect(persistedMode(workspace) == "manual")
        expect(driver0.hover(path, pos) != null)
    }
  }

  private def startInAutoModeTest(): Unit = {

    val (sourceFile, files) = project()

    withWorkspaceServerPositions(
      mode = TestMode.Lsp,
      extraServerOpts = Seq("--jvm", jvm.value, "--auto"),
      timeout = Some(SingleModuleBuildTool.ScalaCli().defaultTimeout)
    )(files*) {
      (workspace, driver, positions, _) =>

        val driver0 = lsp(driver)
        val path    = workspace / sourceFile
        val pos     = positions.lspPos(sourceFile, 1)

        expect(modeOf(driver0.executeCommand("plasmon/mode")) == "automatic")
        expect(persistedMode(workspace) == "automatic")

        // Opening the file is all it takes
        driver0.didOpen(path, 0, os.read(path))
        val hover = awaitHover(driver0, path, pos, 2.minutes)
        expect(hover.getContents.getRight.getValue.contains("greeting"))
    }
  }
}
