package plasmon.integration

import plasmon.integration.TestUtil.*

class PcTweaksTests extends PlasmonSuite {

  for {
    (scalaVersionOpt, serverOpt, buildTool, jvm, testNameSuffix) <-
      scalaVersionBuildToolJvmValues ++ olderScalaVersionBuildToolJvmValues
    if buildTool == SingleModuleBuildTool.ScalaCli()
    scalaVersion <- scalaVersionOpt
    mode         <- modes
  }
    test("Untupling" + testNameSuffix + mode.testNameSuffix) {
      untuplingTest(mode, scalaVersion, jvm, serverOpt)
    }

  for {
    (scalaVersionOpt, serverOpt, buildTool, jvm, testNameSuffix) <-
      scalaVersionBuildToolJvmValues ++ olderScalaVersionBuildToolJvmValues
    if buildTool == SingleModuleBuildTool.ScalaCli()
    scalaVersion <- scalaVersionOpt
    mode         <- modes
  }
    test("Weird method call" + testNameSuffix + mode.testNameSuffix) {
      weirdMethodCallTest(mode, scalaVersion, jvm, serverOpt)
    }

  def untuplingTest(
    mode: TestMode,
    scalaVersion: Labelled[String],
    jvm: Labelled[String],
    serverOpt: Seq[String]
  ): Unit = {
    // Presentation compiler behaviour only - replayed rather than imported for real
    val buildTool = SingleModuleBuildTool.Replayed(
      SingleModuleBuildTool.ScalaCli(),
      os.sub / "pc-tweaks-tests/untupling/scala-cli" /
        s"scala-${scalaVersion.label}" / s"jvm-${jvm.label}"
    )
    val source =
      s"""//> using scala ${scalaVersion.value}
         |//> using jvm ${jvm.value}
         |
         |object Foo {
         |
         |  def input: String = ???
         |
         |  val (s<1>tr, c<2>ount) = input match {
         |    case "" => ("", 2)
         |    case _ => ???
         |  }
         |
         |}
         |""".stripMargin

    val (sourceFile, files) = buildTool.singleFile(os.sub / "Foo.scala", source)

    withWorkspaceServerPositions(
      mode = mode,
      extraServerOpts = Seq("--jvm", jvm.value) ++ serverOpt,
      timeout = Some(buildTool.defaultTimeout)
    )(files*) {
      (workspace, driver, positions, osOpt) =>

        buildTool.setup(workspace, driver, osOpt, compiles = false)

        def hoverAt(pos: Int): Unit =
          checkTextFixture(
            fixtureDir / "plasmon/integration/pc-tweaks-tests/untupling/hover" /
              s"scala-${scalaVersion.label}" / s"jvm-${jvm.label}" / s"pos-$pos.txt",
            hoverMarkdown(
              driver,
              workspace / sourceFile,
              positions.lspPos(sourceFile, pos)
            ),
            osOpt
          )

        def goToDefAt(pos: Int): Unit =
          checkJsoniterFixture(
            fixtureDir / "plasmon/integration/pc-tweaks-tests/untupling/definition" / buildTool.id /
              s"scala-${scalaVersion.label}" / s"jvm-${jvm.label}" / s"definition-$pos.txt",
            goToDef(
              driver,
              workspace,
              workspace / sourceFile,
              positions.lspPos(sourceFile, pos)
            ),
            osOpt
          )

        hoverAt(1)
        hoverAt(2)

        goToDefAt(1)
        goToDefAt(2)
    }
  }

  def weirdMethodCallTest(
    mode: TestMode,
    scalaVersion: Labelled[String],
    jvm: Labelled[String],
    serverOpt: Seq[String]
  ): Unit = {
    // Presentation compiler behaviour only - replayed rather than imported for real
    val buildTool = SingleModuleBuildTool.Replayed(
      SingleModuleBuildTool.ScalaCli(),
      os.sub / "pc-tweaks-tests/weird-method-call/scala-cli" /
        s"scala-${scalaVersion.label}" / s"jvm-${jvm.label}"
    )
    val source =
      s"""//> using scala ${scalaVersion.value}
         |//> using jvm ${jvm.value}
         |
         |object Foo {
         |
         |  def thing[T](
         |    name: String = "",
         |    counts: List[Int]
         |  ): Unit =
         |    ???
         |
         |  th<1>ing(
         |    cou<2>nts = List(2)
         |  )
         |}
         |""".stripMargin

    val (sourceFile, files) = buildTool.singleFile(os.sub / "Foo.scala", source)

    withWorkspaceServerPositions(
      mode = mode,
      extraServerOpts = Seq("--jvm", jvm.value) ++ serverOpt,
      timeout = Some(buildTool.defaultTimeout)
    )(files*) {
      (workspace, driver, positions, osOpt) =>

        buildTool.setup(workspace, driver, osOpt, compiles = false)

        def hoverAt(pos: Int): Unit =
          checkTextFixture(
            fixtureDir / "plasmon/integration/pc-tweaks-tests/weird-method-call/hover" /
              s"scala-${scalaVersion.label}" / s"jvm-${jvm.label}" / s"pos-$pos.txt",
            hoverMarkdown(
              driver,
              workspace / sourceFile,
              positions.lspPos(sourceFile, pos)
            ),
            osOpt
          )

        def goToDefAt(pos: Int): Unit =
          checkJsoniterFixture(
            fixtureDir / "plasmon/integration/pc-tweaks-tests/weird-method-call/definition" / buildTool.id /
              s"scala-${scalaVersion.label}" / s"jvm-${jvm.label}" / s"definition-$pos.txt",
            goToDef(
              driver,
              workspace,
              workspace / sourceFile,
              positions.lspPos(sourceFile, pos)
            ),
            osOpt
          )

        hoverAt(1)
        hoverAt(2)

        goToDefAt(1)
        goToDefAt(2)
    }
  }

}
