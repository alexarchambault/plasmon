package plasmon.integration

import org.eclipse.lsp4j as l
import plasmon.integration.TestUtil.*

import scala.concurrent.duration.IntMult

class BasicTestsJava extends PlasmonSuite {

  // FIXME Use a lib too
  private def fileContentFor(jvm: String): String =
    s"""//> using jvm "$jvm"
       |
       |package foo;
       |
       |import java.io.IOException;
       |import java.io.PrintStream;
       |import java.util.List;
       |
       |public class F<1>oo {
       |  public void foo() {
       |    Syst<2>em.er<3>r.pri<4>ntln("");
       |    Runtim<5>e.g<6>etRuntime().addSh<7>utdownHook(null);
       |  }
       |
       |  private void thing() throws IOException {
       |    Th<8>read.cur<9>rentThread().get<10>ContextClassLoader().get<11>Resources("");
       |    Runt<12>ime.get<13>Runtime().avai<14>lableProcessors();
       |    PrintStream err = Syst<15>em.er<16>r;
       |  }
       |}
       |""".stripMargin

  for (
    (buildTool, jvm, testNameSuffix) <- buildToolJvmValues if buildTool != SingleModuleBuildTool.Sbt
  )
    test("test" + testNameSuffix) {
      mainTest(buildTool, jvm)
    }

  private def mainTest(
    buildTool: SingleModuleBuildTool,
    jvm: Labelled[String]
  ): Unit = {
    val (actualPath, files) = buildTool.singleModule(
      "test-mod",
      Map(
        os.sub / "Foo.java" -> fileContentFor(jvm.value)
      )
    )

    withWorkspaceServerPositions(
      extraServerOpts = Seq("--jvm", jvm.value),
      timeout = Some(2 * buildTool.defaultTimeout)
    )(files*) {
      (workspace, server, positions, osOpt) =>

        buildTool.setup(workspace, osOpt)

        val mainSourceFile = actualPath(os.sub / "Foo.java")

        def hoverAt(pos: Int): Unit =
          checkTextFixture(
            fixtureDir / "plasmon/integration/single-file-tests-java/hover" / buildTool.id /
              s"jvm-${jvm.label}" / s"pos-$pos.txt",
            hoverMarkdown(
              server,
              workspace / mainSourceFile,
              positions.lspPos(mainSourceFile, pos)
            ),
            osOpt
          )

        for (i <- 1 to 16) {
          osOpt.getOrElse(System.err).write(s"Hover $i${System.lineSeparator()}".getBytes)
          hoverAt(i)
        }

        def goToDefAt(pos: Int): Unit =
          checkJsoniterFixture(
            fixtureDir / "plasmon/integration/single-file-tests-java/definition" / buildTool.id /
              s"jvm-${jvm.label}" / s"definition-$pos.txt",
            goToDef(
              server,
              workspace,
              workspace / mainSourceFile,
              positions.lspPos(mainSourceFile, pos)
            ),
            osOpt
          )

        val outputStream = osOpt.getOrElse(System.err)
        val nl           = "\n"
        for (i <- 1 to 15) {
          outputStream.flush()
          outputStream.write(s"Definition $i$nl".getBytes)
          outputStream.flush()
          goToDefAt(i)
        }

        def goToDefInDefAt(
          pos: Either[(String, l.Position), Int],
          actualSourceFile: os.SubPath = mainSourceFile
        ): DefinitionResult = {
          val res = goToDef(
            server,
            workspace,
            workspace / actualSourceFile,
            pos match {
              case Left((_, pos0)) => pos0
              case Right(pos0)     => positions.lspPos(actualSourceFile, pos0)
            }
          )
          checkJsoniterFixture(
            fixtureDir / "plasmon/integration/single-file-tests-java/definition-in-dependencies" / buildTool.id /
              s"jvm-${jvm.label}" / pos.left.map(_._1).map(pos0 => s"definition-$pos0.txt").merge,
            res,
            osOpt
          )
          res
        }

        val goToDefInDefPrintlnRes = goToDefInDefAt(Right(4))
        goToDefInDefAt(
          Left((
            "println-4.txt",
            new l.Position(goToDefInDefPrintlnRes.line, goToDefInDefPrintlnRes.colAverage)
          )),
          actualSourceFile = os.SubPath(goToDefInDefPrintlnRes.path)
        )
    }
  }
}
