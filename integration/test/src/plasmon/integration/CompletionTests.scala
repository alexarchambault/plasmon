package plasmon.integration

import com.eed3si9n.expecty.Expecty.expect
import plasmon.integration.TestUtil.*

class CompletionTests extends PlasmonSuite {
  import CompletionTests.*

  for {
    (scalaVersionOpt, serverOpt, buildTool, jvm, testNameSuffix) <- scalaVersionBuildToolJvmValues
    mode                                                         <- modes
  }
    test("chains" + testNameSuffix + mode.testNameSuffix) {
      completionChainTest(
        mode,
        Seq(
          Seq("sc", "ala.", "co", "llection.", "mu", "table.", "Li", "stBuffer"),
          Seq("Sy", "stem.", "e", "rr.", "pr", "intln($0)"),
          Seq("Ru", "ntime.", "ge", "tRuntime().", "ad", "dShutdownHook($0)")
        ),
        buildTool,
        scalaVersionOpt = scalaVersionOpt,
        jvm = jvm,
        serverOpt = serverOpt
      )
    }

  for {
    (scalaVersionOpt, serverOpt, buildTool, jvm, testNameSuffix) <- scalaVersionBuildToolJvmValues
    mode                                                         <- modes
  }
    test(s"import" + testNameSuffix + mode.testNameSuffix) {
      classPathSearchCompletionTest(
        mode,
        scalaVersionOpt,
        buildTool,
        jvm,
        serverOpt,
        Seq(
          CompletionTest("ListBuffer", "ListBuffe", "list-buffer"),
          CompletionTest("AtomicInteger", "AtomicIntege", "atomic-integer")
        )
      )
    }

  private def classPathSearchCompletionTest(
    mode: TestMode,
    scalaVersionOpt: Option[Labelled[String]],
    buildTool0: SingleModuleBuildTool,
    jvm: Labelled[String],
    serverOpt: Seq[String],
    testInputs: Seq[CompletionTest]
  ): Unit = {
    // Class path search only - replayed rather than imported for real
    val buildTool = SingleModuleBuildTool.Replayed(
      buildTool0,
      os.sub / "completion-tests/import" / buildTool0.id /
        s"scala-${scalaVersionOpt.map(_.label).getOrElse("default")}" / s"jvm-${jvm.label}"
    )

    val header = (
      scalaVersionOpt.map(_.value).map(sv => s"""//> using scala "$sv"""") ++
        Seq(s"""//> using jvm "${jvm.value}"""")
    ).mkString(System.lineSeparator())
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
      mode = mode,
      extraServerOpts = Seq("--jvm", jvm.value) ++ serverOpt,
      timeout = Some(buildTool.defaultTimeout)
    )(files*) {
      (workspace, driver, positions, osOpt) =>

        buildTool.setup(workspace, driver, osOpt, compiles = false)

        for ((testInput, idx) <- testInputs.zipWithIndex) {
          val sourceFile = actualPath(os.sub / s"Foo$idx.scala")

          val completions = completions0(
            driver,
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
    mode: TestMode,
    inputs: Seq[Seq[String]],
    buildTool0: SingleModuleBuildTool,
    scalaVersionOpt: Option[Labelled[String]],
    jvm: Labelled[String],
    serverOpt: Seq[String]
  ): Unit = {

    // Nothing here exercises the build tool itself, only the completions the presentation
    // compiler offers given what the build tool reported, so we replay that rather than
    // importing for real.
    val buildTool = SingleModuleBuildTool.Replayed(
      buildTool0,
      os.sub / "completion-tests/chains" / buildTool0.id /
        s"scala-${scalaVersionOpt.map(_.label).getOrElse("default")}" / s"jvm-${jvm.label}"
    )

    val header = scalaVersionOpt match {
      case Some(scalaVersion) =>
        s"""//> using scala ${scalaVersion.value}
           |//> using jvm ${jvm.value}
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
      mode = mode,
      extraServerOpts = Seq("--jvm", jvm.value, "--suspend-watcher=false") ++ serverOpt,
      timeout = Some(buildTool.defaultTimeout)
    )(files*) {
      (workspace, driver, positions0, osOpt) =>

        buildTool.setup(workspace, driver, osOpt, compiles = false)

        var positions = positions0

        def check(
          sourceFile: os.SubPath,
          updatedContentOpt: Option[(String, Int)],
          expectedNewText: String
        ): Unit = {
          for ((updatedContent, version) <- updatedContentOpt) {
            positions = positions.update(sourceFile, updatedContent)
            driver.didChange(workspace / sourceFile, version, positions.content(sourceFile))
          }

          val completions0 = completions(
            driver,
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
