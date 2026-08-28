package plasmon.integration

import plasmon.integration.TestUtil.*

class DiagnosticsTests extends PlasmonSuite {

  for {
    jvm  <- jvmValues
    mode <- modes
  }
    test(s"simple Java ${jvm.label}" + mode.testNameSuffix) {
      simpleTest(
        mode,
        SingleModuleBuildTool.ScalaCli(),
        (IntegrationConstants.scala213, "2.13"),
        jvm
      )
    }

  private def simpleTest(
    mode: TestMode,
    buildTool: SingleModuleBuildTool,
    scalaVersion: (String, String),
    jvm: Labelled[String]
  ): Unit = {
    val (sourceFile, files) = buildTool.singleFile(
      os.sub / "Foo.scala",
      s"""//> using scala "${scalaVersion._1}"
         |//> using jvm "${jvm.value}"
         |object Foo {
         |  val n = 2
         |  zz
         |  Nil
         |}
         |""".stripMargin
    )
    withWorkspaceServerPositions(
      mode = mode,
      extraServerOpts = Seq("--jvm", jvm.value),
      timeout = Some(buildTool.defaultTimeout)
    )(files*) {
      (workspace, driver, _, osOpt) =>

        buildTool.setup(workspace, driver, osOpt, compiles = false)

        // Asking for a build rather than relying on the one the import happens to trigger: those
        // diagnostics are cleared again when indexing resets its caches, so what is left to be
        // read back afterwards - which is all a CLI can do - is nothing at all.
        driver.compile(workspace / sourceFile)

        // Scaled rather than a fixed wait: the CLI mode reads the diagnostics back from the
        // server rather than being pushed them, and everything else here already scales with
        // PLASMON_TIMEOUT_OVERRIDE
        val diagParams = driver.awaitDiagnostics(workspace / sourceFile, baseTimeout)

        checkGsonFixture(
          fixtureDir / "plasmon/integration/diagnostics-tests/simple" / buildTool.id / s"scala-${scalaVersion._2}" / s"jvm-${jvm.label}" / "publish-diagnostics-params.json",
          diagParams,
          osOpt,
          replaceAll = standardReplacements(workspace)
        )
    }
  }

}
