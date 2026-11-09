package plasmon.integration

import com.eed3si9n.expecty.Expecty.expect
import plasmon.integration.TestUtil.*

class DependencyDefinitionTests extends PlasmonSuite {

  private def jvm = jvmValues.head

  for (mode <- modes)
    test("inline accessors" + mode.testNameSuffix) {
      inlineAccessorsTest(mode)
    }

  // Private and qualified-private members used from inline methods are replaced by synthetic
  // accessors (inline$…) in the typed trees. Go-to-definition and hover should point at the
  // members themselves. Mill's mill.api.Task, browsed from a project depending on Mill, is full
  // of those.
  private def inlineAccessorsTest(mode: TestMode): Unit = {
    val scalaVersion = IntegrationConstants.scala3
    // Presentation compiler behaviour only - replayed rather than imported for real
    val buildTool = SingleModuleBuildTool.Replayed(
      SingleModuleBuildTool.ScalaCli(),
      os.sub / "dependency-definition-tests/inline-accessors/scala-cli" /
        s"scala-$scalaVersion" / s"jvm-${jvm.label}"
    )
    val source =
      s"""//> using scala $scalaVersion
         |//> using jvm ${jvm.value}
         |package foo
         |
         |class Ctx {
         |  private[foo] def hidden: Int = 2
         |}
         |
         |object Lib {
         |  private object Impl {
         |    def value: Int = 1
         |  }
         |  private[foo] def other(n: Int): Int = n
         |  inline def get(ctx: Ctx): Int =
         |    Im<1>pl.va<2>lue + oth<3>er(1) + ctx.hid<4>den
         |}
         |""".stripMargin

    val (sourceFile, files) = buildTool.singleFile(os.sub / "Lib.scala", source)

    withWorkspaceServerPositions(
      mode = mode,
      extraServerOpts = Seq("--jvm", jvm.value),
      timeout = Some(buildTool.defaultTimeout)
    )(files*) {
      (workspace, driver, positions, osOpt) =>

        buildTool.setup(workspace, driver, osOpt, compiles = false)

        val lines = os.read.lines(workspace / sourceFile)
        def lineOf(text: String): Int = {
          val idx = lines.indexWhere(_.contains(text))
          assert(idx >= 0, s"'$text' not found in $sourceFile")
          idx
        }

        def check(pos: Int, expectedLineText: String, expectedName: String): Unit = {
          val res =
            goToDef(driver, workspace, workspace / sourceFile, positions.lspPos(sourceFile, pos))
          expect(res.path == sourceFile.toString)
          expect(res.line == lineOf(expectedLineText))
          expect(res.content == expectedName)

          val hover =
            hoverMarkdown(driver, workspace / sourceFile, positions.lspPos(sourceFile, pos))
          expect(!hover.contains("inline$"))
        }

        check(1, "private object Impl", "Impl")
        check(2, "def value", "value")
        // accessor in the class of the accessed member
        check(3, "private[foo] def other", "other")
        // accessor taking the receiver as first parameter
        check(4, "private[foo] def hidden", "hidden")
    }
  }
}
