package plasmon.integration

import com.eed3si9n.expecty.Expecty.expect
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

  private def scalaSource(value: String) =
    s"""$header
       |object Foo {
       |  val n = 2
       |  val m = $value
       |}
       |""".stripMargin

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

  for (mode <- lspOnlyModes)
    test("diagnostics of former versions of the code" + mode.testNameSuffix) {
      withScalaCliProject(SingleModuleBuildTool.ScalaCli(), scalaSource("n")) {
        (path, driver, client) =>
          val textDocument = new l.TextDocumentIdentifier(path.toNIO.toUri.toASCIIString)
          // Opening the file type checks it too, for its semanticdb, and that can come last. That
          // version has no errors: its empty list of diagnostics points to no code, and can only
          // be told apart from those of the latest version via the code that was type checked.
          driver.didOpen(path, 0, scalaSource("n"), 0, awaitTimeout)
          enablePcDiagnostics(driver)
          // Changes, each followed by a request having the presentation compiler type check its
          // version of the code. The requests queue up while the presentation compiler handles
          // the former ones, and get run latest first, so that former versions get type checked
          // after the latest one.
          val values = (1 to 20).map(i => s"zz$i") :+ "zz"
          val requests =
            for ((value, idx) <- values.zipWithIndex) yield {
              driver.didChange(path, idx + 1, scalaSource(value))
              driver.lsp.getTextDocumentService.hover(
                new l.HoverParams(textDocument, new l.Position(3, 6))
              )
            }
          for (req <- requests)
            req.get()
          val expected = Seq(Diag(4, 10, 4, 12, "Not found: zz"))
          awaitPcDiags(client, path, expected)
          // no diagnostics of former versions after those of the latest one
          val published   = client.publishedDiagnostics(path).map(pcDiags)
          val afterLatest = published.drop(published.indexOf(expected))
          expect(afterLatest.forall(_ == expected))
      }
    }

}
