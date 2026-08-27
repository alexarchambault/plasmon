package plasmon.integration

import com.eed3si9n.expecty.Expecty.expect
import org.eclipse.lsp4j as l
import plasmon.integration.TestUtil.*

import scala.jdk.CollectionConverters.*

class BasicTests extends PlasmonSuite {

  private lazy val defaultScalaVersion =
    if (disableScala2Pc) scala213Compat else scala213

  private lazy val scalaVersions = {
    val scala213Values =
      if (disableScala2Pc) Nil
      else Seq((scala213, Nil))
    scala213Values ++ Seq(
      (scala213Compat, compatServerOpt)
    )
  }

  private def fileContentFor(jvm: String, scalaVersionOpt: Option[String]): String = {
    val scalaVersion = scalaVersionOpt.getOrElse(defaultScalaVersion.value)
    if (scalaVersion.startsWith("2."))
      s"""//> using scala "$scalaVersion"
         |//> using jvm "$jvm"
         |//> using lib "com.lihaoyi::os-lib:0.9.1"
         |
         |import sca<1>la.colle<2>ction.mut<3>able.List<4>Buffer
         |import scala.collection.mut<46>able
         |object B<5>ar {
         |  sca<6>la.colle<7>ction.m<8>utable.ListBuf<9>fer
         |  Syst<10>em.er<11>r.pri<12>ntln("")
         |  Runtim<13>e.g<14>etRuntime.addSh<15>utdownHook(???)
         |  o<16>s.Pa<17>th
         |  o<18>s.p<19>wd
         |}
         |
         |object Thi<20>ng {
         |  import sc<21>ala.jd<22>k.Col<23>lectionConverters._
         |  Th<24>read.cur<25>rentThread().get<26>ContextClassLoader().get<27>Resources("").as<28>Scala.to<29>Vector
         |    .ma<30>p(_.to<31>URI.to<32>ASCIIString)
         |  Runt<33>ime.get<34>Runtime().avai<35>lableProcessors()
         |}
         |
         |object F<36>oo {
         |  def f<37>oo = {
         |    ListBu<38>ffer.em<39>pty[St<40>ring]
         |    Sys<41>tem.e<42>rr.pri<43>ntln("")
         |  }
         |  Syst<44>em.er<45>r
         |}
         |""".stripMargin
    else
      s"""//> using scala "$scalaVersion"
         |//> using jvm "$jvm"
         |//> using lib "com.lihaoyi::os-lib:0.9.1"
         |
         |import sca<1>la.colle<2>ction.mut<3>able.List<4>Buffer
         |import scala.collection.mut<46>able
         |object B<5>ar:
         |  sca<6>la.colle<7>ction.m<8>utable.ListBuf<9>fer
         |  Syst<10>em.er<11>r.pri<12>ntln("")
         |  Runtim<13>e.g<14>etRuntime.addSh<15>utdownHook(???)
         |  o<16>s.Pa<17>th
         |  o<18>s.p<19>wd
         |
         |
         |object Thi<20>ng:
         |  import sc<21>ala.jd<22>k.Col<23>lectionConverters._
         |  Th<24>read.cur<25>rentThread().get<26>ContextClassLoader().get<27>Resources("").as<28>Scala.to<29>Vector
         |    .ma<30>p(_.to<31>URI.to<32>ASCIIString)
         |  Runt<33>ime.get<34>Runtime().avai<35>lableProcessors()
         |
         |
         |object F<36>oo:
         |  def f<37>oo =
         |    ListBu<38>ffer.em<39>pty[St<40>ring]
         |    Sys<41>tem.e<42>rr.pri<43>ntln("")
         |
         |  Syst<44>em.er<45>r
         |
         |""".stripMargin
  }

  for {
    (
      scalaVersionOpt,
      serverOpt,
      buildTool,
      jvm,
      testNameSuffix
    )    <- scalaVersionBuildToolJvmValues0(scripting = true)
    mode <- modes
  }
    test("test" + testNameSuffix + mode.testNameSuffix) {
      mainTest(mode, scalaVersionOpt, buildTool, jvm, serverOpt)
    }

  for {
    (scalaVersion, serverOpt) <- scalaVersions
    mode                      <- modes
  }
    test(
      s"test Scala CLI Scala ${scalaVersion.label} Java ${jvmValues.head.label} twice" +
        mode.testNameSuffix
    ) {
      mainTest(
        mode,
        Some(scalaVersion),
        SingleModuleBuildTool.ScalaCli(),
        jvmValues.head,
        serverOpt,
        count = 2
      )
    }

  private def mainTest(
    mode: TestMode,
    scalaVersionOpt: Option[Labelled[String]],
    buildTool0: SingleModuleBuildTool,
    jvm: Labelled[String],
    serverOpt: Seq[String],
    count: Int = 1
  ): Unit = {
    // Needs built output - it recompiles mid-test after moving a source into a package - so the
    // recording is compiled at replay time rather than a build tool being run.
    //
    // Script mode still runs for real. Its generated wrappers are recorded like any others, but a
    // compile request on a .sc file never reaches the build server, so nothing rebuilds the module
    // and scripts can't see each other's wrapper objects (`Not found: Foo_sc`). Worth revisiting -
    // the recording holds everything needed, it's the compile trigger that's missing.
    val buildTool =
      if (buildTool0.scriptBased) buildTool0
      else
        SingleModuleBuildTool.Replayed(
          buildTool0,
          os.sub / "basic-tests" / buildTool0.id /
            s"scala-${scalaVersionOpt.map(_.label).getOrElse("default")}" / s"jvm-${jvm.label}"
        )

    val header = (
      scalaVersionOpt.map(_.value).map(sv => s"""//> using scala "$sv"""") ++
        Seq(s"""//> using jvm "${jvm.value}"""")
    ).mkString(System.lineSeparator())
    def objectStart(name: String) = if (buildTool.scriptBased) "" else s"object $name {"
    val objectEnd                 = if (buildTool.scriptBased) "" else "}"
    val (actualPath, files) = buildTool.singleModule(
      "test-mod",
      Map(
        os.sub / "Foo.scala" -> fileContentFor(jvm.value, scalaVersionOpt.map(_.value)),
        os.sub / "SigHelp.scala" ->
          s"""$header
             |${objectStart("SigHelp")}
             |  List(<1>)
             |$objectEnd
             |""".stripMargin,
        os.sub / "CodeLensStuff.scala" ->
          s"""
             |${objectStart("CodeLensStuff")}
             |  val a: Iterator[String] =
             |    new Iterator[String] {
             |      def <1>hasNext<2> = true
             |      def <3>next<4>(): String = "a"
             |    }
             |$objectEnd
             |""".stripMargin,
        os.sub / "foo/Definitions.scala" ->
          s"""package foo
             |
             |${objectStart("Definitions")}
             |  def count = 2
             |$objectEnd
             |""".stripMargin,
        os.sub / "foo/Foo.scala" ->
          s"""package foo
             |
             |${objectStart("Foo")}
             |  def apply(): Int = Defin<1>itions.cou<2>nt + 1
             |$objectEnd
             |""".stripMargin
      )
    )
    val clientCapabilities = new l.ClientCapabilities
    clientCapabilities.setWorkspace {
      val cap = new l.WorkspaceClientCapabilities
      cap.setApplyEdit(true)
      cap
    }
    withWorkspaceServerPositionsCount(
      mode = mode,
      clientCapabilities = clientCapabilities,
      extraServerOpts = Seq("--jvm", jvm.value, "--import-persisted-targets=false") ++ serverOpt,
      count = count,
      timeout = Some(buildTool.defaultTimeout)
    )(files*) {
      (workspace, driver, positions, osOpt, runCount) =>

        buildTool.setup(workspace, driver, osOpt, readOnlyToplevelSymbolsCache = runCount > 0)

        val mainSourceFile = actualPath(os.sub / "Foo.scala")

        def hoverAt(pos: Int): Unit =
          checkTextFixture(
            fixtureDir / "plasmon/integration/single-file-tests/hover" / buildTool.id /
              s"scala-${scalaVersionOpt.map(_.label).getOrElse("default")}" / s"jvm-${jvm.label}" / s"pos-$pos.txt",
            hoverMarkdown(
              driver,
              workspace / mainSourceFile,
              positions.lspPos(mainSourceFile, pos)
            ),
            osOpt
          )

        for (i <- (1 to 15) ++ (20 to 46)) {
          osOpt.getOrElse(System.err).write(s"Hover $i${System.lineSeparator()}".getBytes)
          hoverAt(i)
        }

        def goToDefAt(pos: Int): Unit =
          checkJsoniterFixture(
            fixtureDir / "plasmon/integration/single-file-tests/definition" / buildTool.id /
              s"scala-${scalaVersionOpt.map(_.label).getOrElse("default")}" / s"jvm-${jvm.label}" / s"definition-$pos.txt",
            goToDef(
              driver,
              workspace,
              workspace / mainSourceFile,
              positions.lspPos(mainSourceFile, pos)
            ),
            osOpt
          )

        val outputStream = osOpt.getOrElse(System.err)
        val nl           = "\n"
        for (i <- Seq(4) ++ (9 to 15) ++ (23 to 35)) {
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
            driver,
            workspace,
            workspace / actualSourceFile,
            pos match {
              case Left((_, pos0)) => pos0
              case Right(pos0)     => positions.lspPos(actualSourceFile, pos0)
            }
          )
          checkJsoniterFixture(
            fixtureDir / "plasmon/integration/single-file-tests/definition-in-dependencies" / buildTool.id /
              s"scala-${scalaVersionOpt.map(_.label).getOrElse("default")}" / s"jvm-${jvm.label}" / pos.left.map(
                _._1
              ).map(pos0 => s"definition-$pos0.txt").merge,
            res,
            osOpt
          )
          res
        }

        val goToDefInDefPrintlnRes = goToDefInDefAt(Right(12))
        goToDefInDefAt(
          Left((
            "println-27.txt",
            new l.Position(goToDefInDefPrintlnRes.line, goToDefInDefPrintlnRes.colAverage)
          )),
          actualSourceFile = os.SubPath(goToDefInDefPrintlnRes.path)
        )

        val signatureHelpSourceFile = actualPath(os.sub / "SigHelp.scala")
        val signatureHelp = driver.signatureHelp(
          workspace / signatureHelpSourceFile,
          positions.lspPos(signatureHelpSourceFile, 1)
        )

        checkGsonFixture(
          fixtureDir / "plasmon/integration/single-file-tests/signature-help" / buildTool.id /
            s"scala-${scalaVersionOpt.map(_.label).getOrElse("default")}" / s"jvm-${jvm.label}" / (signatureHelpSourceFile / os.up) / s"${signatureHelpSourceFile.last}.lenses",
          signatureHelp,
          osOpt
        )

        for (scalaVersion <- scalaVersionOpt) {
          val lensesSourceFile = actualPath(os.sub / "CodeLensStuff.scala")
          val lenses           = driver.codeLens(workspace / lensesSourceFile).asJava

          checkGsonFixture(
            fixtureDir / "plasmon/integration/single-file-tests/code-lens-go-to-parent" / buildTool.id /
              s"scala-${scalaVersion.label}" / s"jvm-${jvm.label}" / (lensesSourceFile / os.up) / s"${lensesSourceFile.last}.lenses",
            lenses,
            osOpt,
            replaceAll = standardReplacements(workspace),
            roundTrip = true
          )

          if (count == 1) {
            val packageInNewFileSourceFile = actualPath(os.sub / "foo/Foo.scala")
            val newSourceFile              = packageInNewFileSourceFile / os.up / "bar/Bar.scala"
            os.write(workspace / newSourceFile, Array.emptyByteArray, createFolders = true)
            // Opening an empty file in a package directory has the server ask for a package
            // clause to be written into it. The wait for that scales rather than being a fixed
            // 5s: on a loaded runner (the mac job runs three test JVMs, each with a server and a
            // compiler behind it) it can take a while to come back, and every other wait in these
            // tests already scales with PLASMON_TIMEOUT_OVERRIDE.
            val edits = driver.didOpen(
              workspace / newSourceFile,
              version = 0,
              content = "",
              expectedEdits = 1
            )
            expect(edits.length == 1)
            val content = os.read(workspace / newSourceFile)
            expect(content.startsWith("package foo.bar"))

            buildTool.compile(workspace, driver, osOpt)
            driver.index()
            buildTool.compile(workspace, driver, osOpt)
          }
        }

        val sameModuleGoToDefPath = actualPath(os.sub / "foo/Foo.scala")

        val sameModuleGoToDefDir = fixtureDir / "plasmon/integration/single-module/go-to-definition" /
          buildTool.id / s"scala-${scalaVersionOpt.map(_.label).getOrElse("default")}" / s"jvm-${jvm.label}"
        if (!buildTool.scriptBased)
          // doesn't work for now (needs fixing in Scala CLI, so that the sc files semanticdb retain a symbol for the wrapping object)
          checkJsoniterFixture(
            sameModuleGoToDefDir / "obj-definition.json",
            goToDef(
              driver,
              workspace,
              workspace / sameModuleGoToDefPath,
              positions.lspPos(sameModuleGoToDefPath, 1)
            ),
            osOpt
          )
        checkJsoniterFixture(
          sameModuleGoToDefDir / "method-definition.json",
          goToDef(
            driver,
            workspace,
            workspace / sameModuleGoToDefPath,
            positions.lspPos(sameModuleGoToDefPath, 2)
          ),
          osOpt
        )

        // TODO Strip margin helper
        // TODO Pick up serialized build server and targets
        // TODO Status for individual files
        // TODO Index command
        // TODO Find local references
        // TODO Find references
        // TODO Handles didFocus notification
        // TODO Semanticdb in status bar
    }
  }
}
