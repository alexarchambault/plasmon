package plasmon.integration

import org.eclipse.lsp4j as l
import plasmon.integration.TestUtil.*

import java.util.concurrent.{TimeUnit, TimeoutException}

import scala.jdk.CollectionConverters.*

class Tests extends PlasmonSuite {

  // Nothing to run through the CLI here: this is about the LSP connection itself
  test("exit") {
    withWorkspaceAndServer(shutdownServer = false)() {
      (_, driver, listeningFuture, _, _) =>
        val remoteServer = driver.lsp
        def shouldTimeout(): Unit =
          try {
            listeningFuture.get(100L, TimeUnit.MILLISECONDS)
            throw new Exception("Should have timed out")
          }
          catch {
            case _: TimeoutException =>
          }

        shouldTimeout()
        remoteServer.shutdown().get()
        shouldTimeout()
        remoteServer.exit()
        listeningFuture.get(10L, TimeUnit.SECONDS)
    }
  }

  for {
    (scalaVersionOpt, serverOpt, buildTool, jvm, testNameSuffix) <- scalaVersionBuildToolJvmValues
    mode                                                         <- modes
  }
    test("simple" + testNameSuffix + mode.testNameSuffix) {
      simpleTest(mode, buildTool, scalaVersionOpt, jvm, serverOpt)
    }

  private def simpleTest(
    mode: TestMode,
    buildTool0: SingleModuleBuildTool,
    scalaVersionOpt: Option[Labelled[String]],
    jvm: Labelled[String],
    serverOpt: Seq[String]
  ): Unit = {

    // Hover, go-to-definition and completions all resolve against the class path and the
    // workspace source index, neither of which needs a build to have run - replayed.
    val buildTool = SingleModuleBuildTool.Replayed(
      buildTool0,
      os.sub / "tests/simple" / buildTool0.id /
        s"scala-${scalaVersionOpt.map(_.label).getOrElse("default")}" / s"jvm-${jvm.label}"
    )

    val header = scalaVersionOpt.fold("") { scalaVersion =>
      s"""//> using scala "${scalaVersion.value}"
         |//> using jvm "${jvm.value}"
         |""".stripMargin
    }

    val (actualPath, files) = buildTool.singleModule(
      "test-mod",
      Map(
        os.sub / "Hover.scala" ->
          s"""${header}object Hover {
             |  pri<1>ntln("a")
             |}
             |""".stripMargin,
        os.sub / "GoToDef.scala" ->
          s"""object GoToDef {
             |  pri<1>ntln("a")
             |}
             |""".stripMargin,
        os.sub / "Completion.scala" ->
          """object Completion
            |""".stripMargin
      )
    )

    withWorkspaceServerPositions(
      mode = mode,
      extraServerOpts = Seq("--jvm", jvm.value, "--suspend-watcher=false") ++ serverOpt,
      timeout = Some(buildTool.defaultTimeout)
    )(files*) {
      (workspace, driver, positions, osOpt) =>

        buildTool.setup(workspace, driver, osOpt)

        val hoverSourceFile = actualPath(os.sub / "Hover.scala")
        val markdown = hoverMarkdown(
          driver,
          workspace / hoverSourceFile,
          positions.lspPos(hoverSourceFile, 1)
        )
        checkTextFixture(
          fixtureDir / "plasmon/integration/tests/simple-hover" /
            buildTool.id / s"scala-${scalaVersionOpt.map(_.label).getOrElse("default")}" / s"jvm-${jvm.label}" / "hover.txt",
          markdown,
          osOpt
        )

        val goToDefSourceFile = actualPath(os.sub / "GoToDef.scala")
        val goToDefRes = goToDef(
          driver,
          workspace,
          workspace / goToDefSourceFile,
          positions.lspPos(goToDefSourceFile, 1)
        )

        checkJsoniterFixture(
          fixtureDir / "plasmon/integration/tests/simple-go-to-definition" /
            buildTool.id / s"scala-${scalaVersionOpt.map(_.label).getOrElse("default")}" / s"jvm-${jvm.label}" / "definition.json",
          goToDefRes,
          osOpt
        )

        var positions0           = positions
        val completionSourceFile = actualPath(os.sub / "Completion.scala")
        positions0 = positions0.update(
          completionSourceFile,
          s"""object Completion {
             |  println("a")
             |  System.err.pr<1>
             |}
             |""".stripMargin
        )
        driver.lsp.getTextDocumentService.didChange(
          new l.DidChangeTextDocumentParams(
            new l.VersionedTextDocumentIdentifier(
              (workspace / completionSourceFile).toNIO.toUri.toASCIIString,
              2
            ),
            List(
              new l.TextDocumentContentChangeEvent(positions0.content(completionSourceFile))
            ).asJava
          )
        )
        val completions = completions0(
          driver,
          workspace / completionSourceFile,
          positions0.lspPos(completionSourceFile, 1)
        )

        checkGsonFixture(
          fixtureDir / "plasmon/integration/tests/simple-completion" /
            buildTool.id / s"scala-${scalaVersionOpt.map(_.label).getOrElse("default")}" / s"jvm-${jvm.label}" / "completions.json",
          completions,
          osOpt,
          replaceAll = standardReplacements(workspace),
          roundTrip = true
        )
    }
  }
}
