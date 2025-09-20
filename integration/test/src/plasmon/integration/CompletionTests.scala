package plasmon.integration

import com.eed3si9n.expecty.Expecty.expect
import org.eclipse.{lsp4j => l}
import plasmon.integration.TestUtil.*

import scala.jdk.CollectionConverters.*

class CompletionTests extends PlasmonSuite {
  import CompletionTests.*

  for ((scalaVersionOpt, buildTool, jvm, testNameSuffix) <- scalaVersionBuildToolJvmValues)
    test("chains" + testNameSuffix) {
      completionChainTest(
        Seq(
          Seq("sc", "ala.", "co", "llection.", "mu", "table.", "Li", "stBuffer"),
          Seq("Sy", "stem.", "e", "rr.", "pr", "intln($0)"),
          Seq("Ru", "ntime.", "ge", "tRuntime().", "ad", "dShutdownHook($0)")
        ),
        buildTool,
        scalaVersionOpt = scalaVersionOpt,
        jvm = jvm
      )
    }

  for ((scalaVersionOpt, buildTool, jvm, testNameSuffix) <- scalaVersionBuildToolJvmValues)
    test(s"import" + testNameSuffix) {
      classPathSearchCompletionTest(
        scalaVersionOpt,
        buildTool,
        jvm,
        Seq(
          CompletionTest("ListBuffer", "ListBuffe", "list-buffer"),
          CompletionTest("AtomicInteger", "AtomicIntege", "atomic-integer")
        )
      )
    }

  private def classPathSearchCompletionTest(
    scalaVersionOpt: Option[Labelled[String]],
    buildTool: SingleModuleBuildTool,
    jvm: Labelled[String],
    testInputs: Seq[CompletionTest]
  ): Unit = {
    val header = scalaVersionOpt.map(_.value).fold("")(sv => s"""//> using scala "$sv"""")
    val (actualPath, files) = buildTool.singleModule(
      "test-mod",
      testInputs
        .zipWithIndex
        .map {
          case (testInput, idx) =>
            os.sub / s"Foo$idx.scala" ->
              s"""$header
                 |object Foo$idx {
                 |  ${testInput.input}<1>
                 |}
                 |""".stripMargin
        }
        .toMap
    )

    withWorkspaceServerPositions(
      extraServerOpts = Seq("--jvm", jvm.value),
      timeout = Some(buildTool.defaultTimeout)
    )(files: _*) {
      (workspace, remoteServer, positions, osOpt) =>

        buildTool.setup(workspace, osOpt, compiles = false)

        for ((testInput, idx) <- testInputs.zipWithIndex) {
          val sourceFile = actualPath(os.sub / s"Foo$idx.scala")

          val completions = completions0(
            remoteServer,
            workspace / sourceFile,
            positions.lspPos(sourceFile, 1)
          )

          checkGsonFixture(
            fixtureDir / "plasmon/integration/completion-tests" / s"${testInput.fileNamePart}-import" /
              buildTool.id / s"scala-${scalaVersionOpt.map(_.label).getOrElse("default")}" /
              s"jvm-${jvm.label}" / "completions.json",
            completions,
            osOpt,
            replaceAll = standardReplacements(workspace),
            roundTrip = true
          )
        }
    }
  }

  private def completionChainTest(
    inputs: Seq[Seq[String]],
    buildTool: SingleModuleBuildTool,
    scalaVersionOpt: Option[Labelled[String]],
    jvm: Labelled[String]
  ): Unit = {

    val header = scalaVersionOpt match {
      case Some(scalaVersion) =>
        s"""//> using scala ${scalaVersion.value}
           |""".stripMargin
      case None => ""
    }

    val startPosIdx = 1
    val endPosIdx   = 2
    def content(elems: Seq[String], elemsIdx: Int, count: Int): String =
      s"""${header}object Foo$elemsIdx {
         |  ${elems.take(count).mkString}<$startPosIdx>${elems(count)}<$endPosIdx>
         |}
         |""".stripMargin

    val (actualPath, files) = buildTool.singleModule(
      "test-mod",
      inputs
        .zipWithIndex
        .map {
          case (elems, idx) =>
            os.sub / s"Foo$idx.scala" -> content(elems, idx, 0)
        }
        .toMap
    )

    withWorkspaceServerPositions(
      extraServerOpts = Seq("--jvm", jvm.value, "--suspend-watcher=false"),
      timeout = Some(buildTool.defaultTimeout)
    )(files: _*) {
      (workspace, remoteServer, positions0, osOpt) =>

        buildTool.setup(workspace, osOpt, compiles = false)

        var positions = positions0

        def check(
          sourceFile: os.SubPath,
          updatedContentOpt: Option[(String, Int)],
          expectedNewText: String
        ): Unit = {
          for ((updatedContent, version) <- updatedContentOpt) {
            positions = positions.update(sourceFile, updatedContent)
            remoteServer.getTextDocumentService.didChange(
              new l.DidChangeTextDocumentParams(
                new l.VersionedTextDocumentIdentifier(
                  (workspace / sourceFile).toNIO.toUri.toASCIIString,
                  version
                ),
                List(new l.TextDocumentContentChangeEvent(positions.content(sourceFile))).asJava
              )
            )
          }

          val completions0 = completions(
            remoteServer,
            workspace / sourceFile,
            positions.lspPos(sourceFile, endPosIdx)
          )

          val completion = completions0.find(_.newText == expectedNewText).getOrElse {
            pprint.err.log(completions0)
            sys.error(s"No '$expectedNewText' completion found")
          }
          expect(completion.editStart == positions.pos(sourceFile, startPosIdx))
          expect(completion.editEnd == positions.pos(sourceFile, endPosIdx))
        }

        for ((elems, idx) <- inputs.zipWithIndex) {
          val sourceFile = actualPath(os.sub / s"Foo$idx.scala")

          var currentContent = 0
          for (i <- 0 until elems.length by 2) {
            check(
              sourceFile,
              if (i == currentContent) None else Some((content(elems, idx, i), i)),
              elems.drop(i).take(2).mkString.takeWhile(c => c != '.' && c != '[' && c != ' ')
            )
            currentContent = i
          }
        }
    }
  }

}

object CompletionTests {
  private case class CompletionTest(
    className: String,
    input: String,
    fileNamePart: String
  )
}
