package plasmon.integration.project

import org.eclipse.lsp4j as l
import plasmon.integration.PlasmonSuite
import plasmon.integration.TestUtil.*

import java.io.OutputStream

import scala.concurrent.duration.{DurationInt, FiniteDuration}

class MillTests extends PlasmonSuite {

  val millSources = projectsDir / "mill"

  override def munitTimeout: FiniteDuration =
    if (System.getenv("CI") == null)
      5.minutes
    else
      10.minutes

  private def init(workingDir: os.Path, errOpt: Option[OutputStream]): Unit = {
    runServerCommand(workingDir, errOpt)("build-tool", "add", "--mill-via-bloop")
    runServerCommand(workingDir, errOpt)(
      "import",
      "--target",
      s"${workingDir.toIO.toURI.toASCIIString.stripSuffix("/")}/runner/?id=runner"
    )
    runServerCommand(workingDir, errOpt)("bsp", "compile")
  }

  override def test(options: munit.TestOptions)(body: => Any)(implicit loc: munit.Location): Unit =
    super.test(options) {
      os.remove.all(millSources / ".plasmon")
      body
    }(using loc)

  private val source6 = millSources / "runner/src/mill/runner/MillMain.scala"
  private val pos6    = new l.Position(194, 27)
  private val pos6_1  = new l.Position(199, 41)

  private val source7 = millSources / "main/eval/src/mill/eval/EvaluatorCore.scala"
  // private val pos7    = new l.Position(2, 37)
  private val pos7_1 = new l.Position(9, 42)
  private val pos7_2 = new l.Position(10, 26)
  private val pos7_3 = new l.Position(2, 26)

  private val isCi = System.getenv("CI") != null

  if (!isCi)
    test("hover") {
      hoverTest()
    }

  private def hoverTest(): Unit =
    withWorkspaceAndServer(
      timeout = Some(munitTimeout),
      workspaceOpt = Some(millSources),
      extraServerOpts = if (disableScala2Pc) compatServerOpt else Nil
    )() {
      (workingDir, server, _, osOpt, _) =>
        init(workingDir, osOpt)

        val hover0 = hoverMarkdown(
          server,
          millSources / "runner/src/mill/runner/CliImports.scala",
          new l.Position(7, 49)
        )
        checkTextFixture(
          fixtureDir / "plasmon/integration/mill-tests/hover-0.txt",
          hover0,
          osOpt
        )

        //
        //   No hover on the import, not sure why
        //
        // val hover1 = hoverMarkdown(
        //   server,
        //   millSources / "runner/src/mill/runner/CliImports.scala",
        //   new l.Position(2, 25)
        // )
        // checkTextFixture(
        //   fixtureDir / "plasmon/integration/mill-tests/hover-1.txt",
        //   hover1,
        //   osOpt
        // )

        val hover2 = hoverMarkdown(
          server,
          millSources / "runner/src/mill/runner/CliImports.scala",
          new l.Position(7, 78)
        )
        checkTextFixture(
          fixtureDir / "plasmon/integration/mill-tests/hover-2.txt",
          hover2,
          osOpt
        )

        val hover3 = hoverMarkdown(
          server,
          millSources / "runner/src/mill/runner/CliImports.scala",
          new l.Position(7, 26)
        )
        checkTextFixture(
          fixtureDir / "plasmon/integration/mill-tests/hover-3.txt",
          hover3,
          osOpt
        )

        val hover4 = hoverMarkdown(
          server,
          millSources / "runner/src/mill/runner/MillBuild.scala",
          new l.Position(12, 41)
        )
        checkTextFixture(
          fixtureDir / "plasmon/integration/mill-tests/hover-4.txt",
          hover4,
          osOpt
        )

        val hover5 = hoverMarkdown(
          server,
          millSources / "main/client/src/mill/main/client/MillClientMain.java",
          new l.Position(51, 19)
        )
        checkTextFixture(
          fixtureDir / "plasmon/integration/mill-tests/hover-5.txt",
          hover5,
          osOpt
        )

        val hover6 = hoverMarkdown(
          server,
          source6,
          pos6
        )
        checkTextFixture(
          fixtureDir / "plasmon/integration/mill-tests/hover-6.txt",
          hover6,
          osOpt
        )

        val hover6_1 = hoverMarkdown(
          server,
          source6,
          pos6_1
        )
        checkTextFixture(
          fixtureDir / "plasmon/integration/mill-tests/hover-6-1.txt",
          hover6_1,
          osOpt
        )

        // Fails, minimized in ComplexTests."ADT in other module" (see comment in its source fixture)
        // val hover7 = hoverMarkdown(
        //   server,
        //   source7,
        //   pos7
        // )
        // checkTextFixture(
        //   fixtureDir / "plasmon/integration/mill-tests/hover-7.txt",
        //   hover7,
        //   osOpt
        // )

        val hover7_1 = hoverMarkdown(
          server,
          source7,
          pos7_1
        )
        checkTextFixture(
          fixtureDir / "plasmon/integration/mill-tests/hover-7-1.txt",
          hover7_1,
          osOpt
        )

        val hover7_2 = hoverMarkdown(
          server,
          source7,
          pos7_2
        )
        checkTextFixture(
          fixtureDir / "plasmon/integration/mill-tests/hover-7-2.txt",
          hover7_2,
          osOpt
        )

        val hover7_3 = hoverMarkdown(
          server,
          source7,
          pos7_3
        )
        checkTextFixture(
          fixtureDir / "plasmon/integration/mill-tests/hover-7-3.txt",
          hover7_3,
          osOpt
        )
    }

  // test("definition") {
  //   withWorkspaceAndServer(timeout = Some(munitTimeout), workspaceOpt = Some(millSources))() {
  //     (workingDir, server, _, osOpt, _) =>
  //       init(workingDir, osOpt)
  //
  //       checkJsoniterFixture(
  //         fixtureDir / "plasmon" / "integration" / "mill-tests" / "definition" / "definition-6.txt",
  //         goToDef(server, millSources, source6, pos6),
  //         osOpt
  //       )
  //
  //       checkJsoniterFixture(
  //         fixtureDir / "plasmon" / "integration" / "mill-tests" / "definition" / "definition-6-1.txt",
  //         goToDef(server, millSources, source6, pos6_1),
  //         osOpt
  //       )
  //
  //       checkJsoniterFixture(
  //         fixtureDir / "plasmon" / "integration" / "mill-tests" / "definition" / "definition-7.txt",
  //         goToDef(server, millSources, source7, pos7),
  //         osOpt
  //       )
  //   }
  // }
}
