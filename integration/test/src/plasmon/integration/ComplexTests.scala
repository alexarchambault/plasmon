package plasmon.integration

import com.eed3si9n.expecty.Expecty.expect
import plasmon.integration.TestUtil.*

class ComplexTests extends PlasmonSuite {

  // LSP only, like the rest of the completion checks: what is under test is what the presentation
  // compiler offers, not how it was asked - see CompletionTests
  for {
    (scalaVersionOpt, serverOpt, buildTool, jvm, testNameSuffix) <- scalaVersionBuildToolJvmValues
    mode                                                         <- lspOnlyModes
  }
    test(testNameSuffix.dropWhile(_.isSpaceChar) + mode.testNameSuffix) {
      complexTest(mode, buildTool, scalaVersionOpt, jvm, serverOpt)
    }

  def complexTest(
    mode: TestMode,
    buildTool0: SingleModuleBuildTool,
    scalaVersionOpt: Option[Labelled[String]],
    jvm: Labelled[String],
    serverOpt: Seq[String]
  ): Unit = {
    // Completions only - replayed rather than imported for real
    val buildTool = SingleModuleBuildTool.Replayed(
      buildTool0,
      os.sub / "complex-tests/completions" / buildTool0.id /
        s"scala-${scalaVersionOpt.map(_.label).getOrElse("default")}" / s"jvm-${jvm.label}"
    )

    val header = scalaVersionOpt match {
      case Some(scalaVersion) =>
        s"""//> using scala ${scalaVersion.value}
           |//> using jvm ${jvm.value}
           |""".stripMargin
      case None => ""
    }
    val source =
      s"""${header}object Foo {
         |  def foo = {
         |    scala.collection.<0>mu<1>
         |    scala.collection.mutable.<2>Li<3>
         |  }
         |  <4>prin<5>
         |  println("a")
         |  scala.collection.<6>i<7>
         |  scala.collection.immutable.<8>L<9>
         |  val s = "foo"
         |  System.out.println("false")
         |  System.err.<10>
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

        def completionAtPos(pos: Int): Unit = {

          val completions = completions0(
            driver,
            workspace / sourceFile,
            positions.lspPos(sourceFile, pos)
          )

          def path(suffix: String = "") =
            fixtureDir / "plasmon/integration/complex-tests" / buildTool.id /
              s"scala-${scalaVersionOpt.map(_.label).getOrElse("default")}" / s"jvm-${jvm.label}" /
              (sourceFile / os.up) /
              s"completions-$pos${if (suffix.isEmpty) "" else "-" + suffix}.json"

          expect {
            (completions.isLeft && completions.getLeft.size() != 0) ||
            (completions.isRight && completions.getRight.getItems.size() != 0)
          }

          checkGsonFixture(
            path(),
            completions,
            osOpt,
            replaceAll = standardReplacements(workspace),
            roundTrip = true
          )
        }

        completionAtPos(1)
        completionAtPos(3)
        completionAtPos(5)
        completionAtPos(7)
        completionAtPos(9)
        completionAtPos(10)
    }
  }

  for {
    (scalaVersionOpt, serverOpt, buildTool, jvm, testNameSuffix) <- scalaVersionBuildToolJvmValues
    if buildTool.id == "mill"
    scalaVersion <- scalaVersionOpt
    mode         <- modes
  }
    test(
      "ADT in other module " + testNameSuffix.dropWhile(_.isSpaceChar) + mode.testNameSuffix
    ) {
      adtInOtherModuleTest(mode, scalaVersion, jvm, serverOpt)
    }

  // CLI only: over LSP, an editor opens files, and opened files get compiled - from a terminal,
  // `module load` is all there is to build what hovers then resolve against
  for (mode <- cliOnlyModes)
    test("ADT in other module, compiled by module load" + mode.testNameSuffix) {
      adtInOtherModuleModuleLoadTest()
    }

  for (mode <- cliOnlyModes)
    test("module load of a file whose module is loaded" + mode.testNameSuffix) {
      moduleAlreadyLoadedTest()
    }

  private def adtMainFile = os.sub / "bar/src/bar/Test.scala"

  private def adtInOtherModuleFiles(scalaVersion: Labelled[String]): Seq[(os.SubPath, String)] = {
    val mainFile = adtMainFile
    Seq(
      os.sub / "build.mill" ->
        s"""import mill._
           |import mill.scalalib._
           |
           |object foo extends ScalaModule {
           |  def scalaVersion = "${scalaVersion.value}"
           |}
           |
           |object bar extends ScalaModule {
           |  def moduleDeps = Seq(foo)
           |  def scalaVersion = "${scalaVersion.value}"
           |}
           |""".stripMargin,
      os.sub / "foo/src/foo/Thing.scala" ->
        """package foo
          |
          |sealed trait Thing
          |
          |object Thing {
          |  case object First extends Thing
          |  case class Second() extends Thing
          |  object Thirds // remove this line, the hover over the Thirds import in the file below fails
          |  sealed trait Thirds extends Thing
          |  case class ThirdOne() extends Thirds
          |}
          |""".stripMargin,
      mainFile ->
        """package bar
          |
          |import foo.Thing.{Fi<1>rst, Sec<2>ond, Thi<3>rdOne, Th<4>irds}
          |
          |object Test {
          |  def test(): Unit = {
          |    println(Fi<11>rst)
          |    println(Se<12>cond())
          |    println(Th<13>irdOne() : Th<14>irds)
          |  }
          |}
          |""".stripMargin
    )
  }

  private def adtRecording(scalaVersion: Labelled[String], jvm: Labelled[String]) =
    SingleModuleBuildTool.Replayed(
      SingleModuleBuildTool.Mill,
      os.sub / "complex-tests/adt-in-other-module" /
        s"scala-${scalaVersion.label}" / s"jvm-${jvm.label}"
    )

  private def adtFixtureDir(scalaVersion: Labelled[String], jvm: Labelled[String]) =
    fixtureDir / "plasmon/integration/complex-tests/other/adt-in-other-module" /
      s"jvm-${jvm.label}" /
      s"scala-${scalaVersion.label}"

  def adtInOtherModuleTest(
    mode: TestMode,
    scalaVersion: Labelled[String],
    jvm: Labelled[String],
    serverOpt: Seq[String]
  ): Unit = {
    val mainFile       = adtMainFile
    val files          = adtInOtherModuleFiles(scalaVersion)
    val testFixtureDir = adtFixtureDir(scalaVersion, jvm)
    withWorkspaceServerPositions(
      mode = mode,
      extraServerOpts = Seq("--jvm", jvm.value) ++ serverOpt,
      timeout = Some(SingleModuleBuildTool.Mill.defaultTimeout)
    )(files*) {
      (workspace, driver, positions, osOpt) =>
        // Cross-module: hovers here resolve symbols defined in the other module, so the recording
        // is compiled at replay time to give them real class files to resolve against
        adtRecording(scalaVersion, jvm).setup(
          workspace,
          driver,
          osOpt,
          readOnlyToplevelSymbolsCache = false,
          compiles = true
        )

        for (i <- (1 to 4).iterator ++ (11 to 14).iterator) {
          val hover = hoverMarkdown(
            driver,
            workspace / mainFile,
            positions.lspPos(mainFile, i)
          )
          checkTextFixture(
            testFixtureDir / s"hover-$i.txt",
            hover,
            osOpt
          )
        }
    }
  }

  /** `module load`, then hovers - no `compile` in between.
    *
    * The module loaded is `bar`, and what is hovered is defined in `foo`, which `bar` depends on:
    * resolving it takes `foo`'s class directory. Before `module load` compiled the module it loads,
    * nothing would have built it, and these hovers came back as `package ` rather than the ADT
    * cases - quietly wrong rather than failing.
    */
  private def adtInOtherModuleModuleLoadTest(): Unit = {
    val scalaVersion   = Labelled("3", IntegrationConstants.scala3)
    val jvm            = jvmValues.head
    val mainFile       = adtMainFile
    val testFixtureDir = adtFixtureDir(scalaVersion, jvm)
    val recording      = adtRecording(scalaVersion, jvm)

    withWorkspaceServerPositions(
      mode = TestMode.Cli,
      extraServerOpts = Seq("--jvm", jvm.value),
      timeout = Some(SingleModuleBuildTool.Mill.defaultTimeout)
    )(adtInOtherModuleFiles(scalaVersion)*) {
      (workspace, driver, positions, osOpt) =>
        BspDataFixture.install(workspace, recording.key)
        driver.loadBuildTool(
          BspDataFixture.replayBuildToolId,
          BspDataFixture.replayBuildToolId,
          workspace / mainFile
        )
        // What the test is about: no driver.compile after this
        driver.loadModuleOf(workspace / mainFile)

        for (i <- (1 to 4).iterator ++ (11 to 14).iterator) {
          val hover = hoverMarkdown(
            driver,
            workspace / mainFile,
            positions.lspPos(mainFile, i)
          )
          checkTextFixture(
            testFixtureDir / s"hover-$i.txt",
            hover,
            osOpt
          )
        }
    }
  }

  /** `module list` / `module load` on files whose modules are already loaded.
    *
    * Both used to leave loaded modules out entirely: `module list` came back empty, and `module
    * load` failed with "No module found" - or, for a file in several cross versions, loaded another
    * one, which then served the file instead of the one already loaded.
    */
  private def moduleAlreadyLoadedTest(): Unit = {
    val scalaVersion = Labelled("3", IntegrationConstants.scala3)
    val jvm          = jvmValues.head
    val mainFile     = adtMainFile
    val otherFile    = os.sub / "foo/src/foo/Thing.scala"

    withWorkspaceServerPositions(
      mode = TestMode.Cli,
      extraServerOpts = Seq("--jvm", jvm.value),
      timeout = Some(SingleModuleBuildTool.Mill.defaultTimeout)
    )(adtInOtherModuleFiles(scalaVersion)*) {
      (workspace, driver, _, _) =>
        val driver0 = driver match {
          case cli: ServerDriver.Cli => cli
          case other                 => sys.error(s"Expected a CLI driver, got $other")
        }
        BspDataFixture.install(workspace, adtRecording(scalaVersion, jvm).key)
        driver.loadBuildTool(
          BspDataFixture.replayBuildToolId,
          BspDataFixture.replayBuildToolId,
          workspace / mainFile
        )

        def modules(file: os.SubPath): Seq[(String, Boolean)] =
          driver0.listModulesOf(workspace / file).map { obj =>
            (
              obj.get("uri").getAsString.stripSuffix("/").split('/').last,
              obj.get("alreadyLoaded").getAsBoolean
            )
          }

        expect(modules(mainFile) == Seq(("bar", false)))

        val firstLoad = driver0.loadModuleOfOutput(workspace / mainFile)
        expect(firstLoad.contains("Loaded module"))

        // Listed, flagged as loaded - not left out. foo too: loading bar loads what it depends on
        expect(modules(mainFile) == Seq(("bar", true)))
        expect(modules(otherFile) == Seq(("foo", true)))

        // The CLI says what's recommended or loaded in words - VS Code codicons such as `$(tag)`
        // are for editors to render, not for a terminal
        val listed = driver0.listModulesOfText(workspace / mainFile)
        expect(listed.contains("[recommended]"))
        expect(listed.contains("[loaded]"))
        expect(!listed.contains("$("))
        expect(driver0.listModulesOf(workspace / mainFile).forall { obj =>
          obj.get("label").getAsString == "bar" && obj.get("recommended").getAsBoolean
        })

        // Said to be loaded already, rather than failing, or loading something else - whether
        // loaded by hand, or along with a module depending on it
        for (file <- Seq(mainFile, otherFile)) {
          val output = driver0.loadModuleOfOutput(workspace / file)
          expect(output.contains("already loaded"))
          expect(!output.contains("Loaded module"))
        }
    }
  }

  for {
    jvm  <- jvmValues
    mode <- modes
  }
    test(s"Java classes in other files and modules Mill Java ${jvm.label}" + mode.testNameSuffix) {
      javaOtherFilesTest(mode, jvm)
    }

  def javaOtherFilesTest(
    mode: TestMode,
    jvm: Labelled[String]
  ): Unit = {
    val mainFile = os.sub / "launcher/src/launcher/Launcher.java"

    val files = Seq(
      os.sub / "build.mill" ->
        """import mill._
          |import mill.javalib._
          |
          |object paths extends JavaModule
          |
          |object launcher extends JavaModule {
          |  def moduleDeps = Seq(paths)
          |}
          |""".stripMargin,
      os.sub / "paths/src/paths/Util.java" ->
        """package paths;
          |
          |public final class Util {
          |  public static String createDirectories(String path) {
          |    return path;
          |  }
          |}
          |""".stripMargin,
      os.sub / "launcher/src/launcher/Download.java" ->
        """package launcher;
          |
          |final class Download {
          |  static String getLocalURLs(String url) {
          |    return url;
          |  }
          |}
          |""".stripMargin,
      mainFile ->
        """package launcher;
          |
          |import paths.U<1>til;
          |
          |public class Launcher {
          |  public static void main(String[] args) {
          |    String a = Downl<2>oad.getLoc<3>alURLs("a");
          |    String b = Ut<4>il.createDir<5>ectories(a);
          |    String c = paths.Ut<6>il.create<7>Directories(b);
          |  }
          |}
          |""".stripMargin
    )

    val testFixtureDir =
      fixtureDir / "plasmon/integration/complex-tests/other/java-other-files" /
        s"jvm-${jvm.label}"
    withWorkspaceServerPositions(
      mode = mode,
      extraServerOpts = Seq("--jvm", jvm.value),
      timeout = Some(SingleModuleBuildTool.Mill.defaultTimeout)
    )(files*) {
      (workspace, driver, positions, osOpt) =>
        // Not compiling: javac should find the other workspace classes in their sources
        SingleModuleBuildTool.Replayed(
          SingleModuleBuildTool.Mill,
          os.sub / "complex-tests/java-other-files" / s"jvm-${jvm.label}"
        ).setup(
          workspace,
          driver,
          osOpt,
          readOnlyToplevelSymbolsCache = false,
          compiles = false
        )

        for (i <- 1 to 7)
          checkJsoniterFixture(
            testFixtureDir / s"definition-$i.txt",
            goToDefs(
              driver,
              workspace,
              workspace / mainFile,
              positions.lspPos(mainFile, i)
            ),
            osOpt
          )
    }
  }

}
