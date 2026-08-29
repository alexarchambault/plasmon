package plasmon.integration

import com.eed3si9n.expecty.Expecty.expect
import plasmon.integration.TestUtil.*

/** `--auto` on the `lsp …` commands: loading whatever a file needs before answering for it.
  *
  * CLI-only by nature. Over LSP there is nothing to test - an editor has loaded a build tool and a
  * module of its own accord long before it asks for a hover, which is exactly the step a terminal
  * has nobody to have taken. So these run through [[ServerDriver.Cli]] alone, and reach into it
  * rather than going through [[ServerDriver]], which has no `--auto` to expose.
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
}
