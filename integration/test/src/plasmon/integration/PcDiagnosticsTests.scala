package plasmon.integration

import org.eclipse.lsp4j as l
import plasmon.integration.TestUtil.*

/** Diagnostics computed by presentation compilers, as published to editors once enabled via
  * `plasmon/enablePcDiagnostics`
  */
class PcDiagnosticsTests extends PlasmonSuite {

  private def jvm = jvmValues.head

  private final case class Diag(
    startLine: Int,
    startChar: Int,
    endLine: Int,
    endChar: Int,
    message: String
  )

  private def pcDiags(diags: Seq[l.Diagnostic]): Seq[Diag] =
    diags
      .filter(diag => Option(diag.getSource).exists(_.endsWith(" presentation compiler")))
      .map { diag =>
        val range = diag.getRange
        val message =
          if (diag.getMessage.isLeft) diag.getMessage.getLeft
          else diag.getMessage.getRight.getValue
        Diag(
          range.getStart.getLine,
          range.getStart.getCharacter,
          range.getEnd.getLine,
          range.getEnd.getCharacter,
          message
        )
      }

  // a fraction of the timeout of the whole test, so that a failure says what went wrong
  private def awaitTimeout = baseTimeout / 3

  /** Waits for the latest presentation compiler diagnostics published for `path` to be `expected`
    */
  private def awaitPcDiags(
    client: MockLanguageClient,
    path: os.Path,
    expected: Seq[Diag]
  ): Unit =
    client.awaitLatestDiagnostics(path, awaitTimeout)(pcDiags(_) == expected) match {
      case Right(_) =>
      case Left(latestOpt) =>
        sys.error(
          s"Expected presentation compiler diagnostics $expected for $path, " +
            s"got ${latestOpt.map(pcDiags)} after $awaitTimeout"
        )
    }

  private def enablePcDiagnostics(driver: ServerDriver.Lsp): Unit =
    driver.executeCommand("plasmon/enablePcDiagnostics", java.lang.Boolean.TRUE)

  private def withScalaCliProject[T](
    buildTool: SingleModuleBuildTool.ScalaCli,
    source: String
  )(f: (os.Path, ServerDriver.Lsp, MockLanguageClient) => T): T = {
    val client              = new MockLanguageClient {}
    val (sourceFile, files) = buildTool.singleFile(os.sub / "Foo.scala", source)
    withWorkspaceServerPositions(
      client = client,
      extraServerOpts = Seq("--jvm", jvm.value),
      timeout = Some(buildTool.defaultTimeout)
    )(files*) {
      (workspace, driver, _, osOpt) =>
        val lspDriver = driver match {
          case lsp: ServerDriver.Lsp => lsp
          case other                 => sys.error(s"Expected an LSP driver, got $other")
        }
        buildTool.setup(workspace, driver, osOpt, compiles = false)
        f(workspace / sourceFile, lspDriver, client)
    }
  }

  private def header =
    s"""//> using scala "${IntegrationConstants.scala3}"
       |//> using jvm "${jvm.value}"""".stripMargin

  for (mode <- lspOnlyModes)
    test("wrapped source" + mode.testNameSuffix) {
      val buildTool = SingleModuleBuildTool.ScalaCli(scriptMode = true)
      val source =
        s"""$header
           |val n = 2
           |val m = zz
           |""".stripMargin
      withScalaCliProject(buildTool, source) {
        (path, driver, client) =>
          enablePcDiagnostics(driver)
          // has the presentation compiler type check the script, which reports its diagnostics
          driver.hover(path, new l.Position(2, 4))
          // reported on the script itself, at the position of the error in it, and only for the
          // code of the user, not for the code wrapping it
          awaitPcDiags(client, path, Seq(Diag(3, 8, 3, 10, "Not found: zz")))
      }
    }

}
