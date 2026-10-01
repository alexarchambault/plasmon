package plasmon.integration

import com.eed3si9n.expecty.Expecty.expect
import plasmon.integration.TestUtil.*

import java.util.concurrent.{TimeUnit, TimeoutException}

class Tests extends PlasmonSuite {

  // Nothing to run through the CLI here: this is about the LSP connection itself
  test("exit") {
    withLspServer(shutdownServer = false)() {
      (_, driver, _) =>
        def shouldTimeout(): Unit =
          try {
            driver.listening.get(100L, TimeUnit.MILLISECONDS)
            throw new Exception("Should have timed out")
          }
          catch {
            case _: TimeoutException =>
          }

        shouldTimeout()
        driver.lsp.shutdown().get()
        shouldTimeout()
        driver.lsp.exit()
        driver.listening.get(10L, TimeUnit.SECONDS)
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

        driver match {
          case cli: ServerDriver.Cli =>
            // Same definition, pointing inside the source JAR rather than at its extracted copy
            val locations = cli.definitionArchiveUris(
              workspace / goToDefSourceFile,
              positions.lspPos(goToDefSourceFile, 1)
            )
            expect(locations.length >= 1)
            val uri = locations.head.getUri
            val (jarUri, entry) = uri.split("!", 2) match {
              case Array(jarUri0, entry0) => (jarUri0, entry0)
              case _                      => sys.error(s"Expected an archive URI, got $uri")
            }
            val jar = os.Path(java.nio.file.Paths.get(new java.net.URI(jarUri)))
            expect(os.isFile(jar))
            expect(jar.last.endsWith("-sources.jar"))
            expect(goToDefRes.path.endsWith("/" + jar.last + "/" + entry))
            val entryContent = {
              val zf = new java.util.zip.ZipFile(jar.toIO)
              try new String(zf.getInputStream(zf.getEntry(entry)).readAllBytes(), "UTF-8")
              finally zf.close()
            }
            expect(entryContent == os.read(workspace / os.SubPath(goToDefRes.path)))
            expect(locations.head.getRange.getStart.getLine == goToDefRes.line)

            // Archive URIs are accepted back, as --uri or as a path, standing for the extracted copy
            val defPos = new org.eclipse.lsp4j.Position(goToDefRes.line, goToDefRes.colAverage)
            val expectedHover = driver.hover(workspace / os.SubPath(goToDefRes.path), defPos)
            expect(expectedHover != null)
            val uriHover  = cli.hoverRaw(Seq("--uri", uri), defPos)
            val pathHover = cli.hoverRaw(Seq(s"$jar!$entry"), defPos)
            expect(uriHover == expectedHover)
            expect(pathHover == expectedHover)
          case _ =>
        }

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
        driver.didChange(
          workspace / completionSourceFile,
          version = 2,
          positions0.content(completionSourceFile)
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
