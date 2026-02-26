package plasmon.integration.project

import com.google.gson.Gson
import io.github.alexarchambault.testutil.TestOutput.FixedReadBytes
import org.eclipse.lsp4j as l
import plasmon.integration.PlasmonSuite
import plasmon.integration.TestUtil.*

import java.io.OutputStream

import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.util.Properties

class CoursierTests extends PlasmonSuite {

  val coursierSources      = projectsDir / "coursier"
  def coursierScalaVersion = "2.13.12"

  override def munitTimeout: FiniteDuration = 5.minutes

  private def bspInit(errOpt: Option[OutputStream]): Unit = {
    val ext = if (Properties.isWin) ".bat" else ""
    os.remove.all(coursierSources / ".bsp")
    os.proc(coursierSources / s"mill$ext", "-i", "mill.bsp.BSP/install").call(
      cwd = coursierSources,
      stdin = os.Inherit,
      stdout = FixedReadBytes.pipeTo(errOpt),
      mergeErrIntoOut = errOpt.nonEmpty
    )
  }

  override def test(options: munit.TestOptions)(body: => Any)(implicit loc: munit.Location): Unit =
    super.test(options) {
      os.remove.all(coursierSources / ".plasmon")
      body
    }(using loc)

  private val isCi = System.getenv("CI") != null

  if (!isCi)
    test("main") {
      mainTest()
    }

  private def mainTest(): Unit =
    withWorkspaceAndServer(timeout = Some(munitTimeout), workspaceOpt = Some(coursierSources))() {
      (workingDir, server, _, osOpt, _) =>
        bspInit(osOpt)

        runServerCommand(workingDir, osOpt)(
          "bsp",
          "add",
          coursierSources
        )

        runServerCommand(workingDir, osOpt)(
          "import",
          coursierSources,
          "--ignore-toplevel-symbols-errors=false",
          "--target",
          s"coursier/jvm/$coursierScalaVersion/test",
          "--target",
          s"core/jvm/$coursierScalaVersion/test"
        )

        // issues with the bsp clean command in mill for now…
        os.remove.all(coursierSources / "out/core")

        runServerCommand(workingDir, osOpt)(
          "bsp",
          "compile",
          "--workspace",
          coursierSources,
          "--target",
          s"coursier/jvm/$coursierScalaVersion/test",
          "--target",
          s"core/jvm/$coursierScalaVersion/test"
        )

        val jsonDiag = serverCommandOutput(workingDir, osOpt)(
          "diagnostics",
          "--json",
          coursierSources / "modules/core/shared/src/main/scala/coursier/core/Dependency.scala"
        )

        val diagnostics = new Gson().fromJson(jsonDiag, classOf[Array[l.Diagnostic]])

        checkGsonFixture(
          fixtureDir / "plasmon/integration/coursier-tests/diagnostics.json",
          diagnostics,
          osOpt,
          roundTrip = true
        )

        val hover0 = hoverMarkdown(
          server,
          coursierSources / "modules/coursier/shared/src/test/scala/coursier/tests/TestHelpers.scala",
          new l.Position(145, 30)
        )
        checkTextFixture(
          fixtureDir / "plasmon/integration/coursier-tests/hover-0.txt",
          hover0,
          osOpt
        )

        val hover1 = hoverMarkdown(
          server,
          coursierSources / "modules/coursier/shared/src/test/scala/coursier/tests/ResolveTests.scala",
          new l.Position(148, 18)
        )
        checkTextFixture(
          fixtureDir / "plasmon/integration/coursier-tests/hover-1.txt",
          hover1,
          osOpt
        )

        runServerCommand(workingDir, osOpt)(
          "check",
          coursierSources / "modules/coursier/shared/src/test/scala/coursier/tests/ResolveTests.scala",
          "--require",
          "scala-2.13"
        )

        val consoleDimPath =
          coursierSources / "modules/cache/jvm/src/main/scala/coursier/cache/internal/ConsoleDim.scala"
        runServerCommand(workingDir, osOpt)("check", consoleDimPath, "--require", "scala-2.13")

        val consoleDimHover = hoverMarkdown(server, consoleDimPath, new l.Position(13, 16))

        checkTextFixture(
          fixtureDir / "plasmon/integration/coursier-tests/load-hover.txt",
          consoleDimHover,
          osOpt
        )
    }
}
