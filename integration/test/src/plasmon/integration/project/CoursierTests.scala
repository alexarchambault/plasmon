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

  val coursierSources = projectsDir / "coursier"

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

        val dependencySource =
          coursierSources / "modules/core/shared/src/main/scala/coursier/core/Dependency.scala"
        val testHelpersSource =
          coursierSources / "modules/coursier/shared/src/test/scala/coursier/tests/TestHelpers.scala"

        loadBuildToolViaLsp(server, "bloop", "bloop", dependencySource)
        loadModuleOfViaLsp(server, dependencySource)
        loadModuleOfViaLsp(server, testHelpersSource)

        // issues with the bsp clean command in mill for now…
        os.remove.all(coursierSources / "out/core")

        compileViaLsp(server, dependencySource)
        compileViaLsp(server, testHelpersSource)

        val jsonDiag = serverCommandOutput(workingDir, osOpt)(
          "diagnostics",
          "--json",
          dependencySource
        )

        val diagnostics = new Gson().fromJson(jsonDiag, classOf[Array[l.Diagnostic]])

        checkGsonFixture(
          fixtureDir / "plasmon/integration/project/coursier-tests/diagnostics.json",
          diagnostics,
          osOpt,
          roundTrip = true
        )

        val hover0 = hoverMarkdown(
          server,
          testHelpersSource,
          new l.Position(145, 30)
        )
        checkTextFixture(
          fixtureDir / "plasmon/integration/project/coursier-tests/hover-0.txt",
          hover0,
          osOpt
        )

        val hover1 = hoverMarkdown(
          server,
          coursierSources / "modules/coursier/shared/src/test/scala/coursier/tests/ResolveTests.scala",
          new l.Position(148, 18)
        )
        checkTextFixture(
          fixtureDir / "plasmon/integration/project/coursier-tests/hover-1.txt",
          hover1,
          osOpt
        )

        compileViaLsp(
          server,
          coursierSources / "modules/coursier/shared/src/test/scala/coursier/tests/ResolveTests.scala"
        )

        val consoleDimPath =
          coursierSources / "modules/cache/jvm/src/main/scala/coursier/cache/internal/ConsoleDim.scala"
        loadModuleOfViaLsp(server, consoleDimPath)
        compileViaLsp(server, consoleDimPath)

        val consoleDimHover = hoverMarkdown(server, consoleDimPath, new l.Position(13, 16))

        checkTextFixture(
          fixtureDir / "plasmon/integration/project/coursier-tests/load-hover.txt",
          consoleDimHover,
          osOpt
        )
    }
}
