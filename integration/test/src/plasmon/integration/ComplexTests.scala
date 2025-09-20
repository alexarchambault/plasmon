package plasmon.integration

import com.eed3si9n.expecty.Expecty.expect
import plasmon.integration.TestUtil.*

class ComplexTests extends PlasmonSuite {

  for ((scalaVersionOpt, buildTool, jvm, testNameSuffix) <- scalaVersionBuildToolJvmValues)
    test(testNameSuffix.dropWhile(_.isSpaceChar)) {
      complexTest(buildTool, scalaVersionOpt, jvm)
    }

  def complexTest(
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
      extraServerOpts = Seq("--jvm", jvm.value),
      timeout = Some(buildTool.defaultTimeout)
    )(files: _*) {
      (workspace, remoteServer, positions, osOpt) =>

        buildTool.setup(workspace, osOpt, compiles = false)

        def completionAtPos(pos: Int, deterministicScala3: Boolean = true): Unit = {

          val completions = completions0(
            remoteServer,
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
            alternativePaths =
              if (!deterministicScala3 && scalaVersionOpt.exists(_.value.startsWith("3.")))
                (1 to 5).map(_.toString).map(path(_))
              else
                Nil,
            roundTrip = true
          )
        }

        completionAtPos(1)
        completionAtPos(3, deterministicScala3 = false)
        completionAtPos(5)
        completionAtPos(7)
        completionAtPos(9, deterministicScala3 = false)
        completionAtPos(10)
    }
  }

  test("ADT in other module") {
    val scalaVersion = "2.13.14"
    val jvm          = jvmValues.find(_.label == "17").getOrElse(???)

    val mainFile = os.sub / "bar/src/bar/Test.scala"

    val files = Seq(
      os.sub / "build.sc" ->
        s"""import mill._
           |import mill.scalalib._
           |
           |object foo extends ScalaModule {
           |  def scalaVersion = "$scalaVersion"
           |}
           |
           |object bar extends ScalaModule {
           |  def moduleDeps = Seq(foo)
           |  def scalaVersion = "$scalaVersion"
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

    val testFixtureDir = fixtureDir / "plasmon/integration/complex-tests/other/adt-in-other-module"
    withWorkspaceServerPositions(
      extraServerOpts = Seq("--jvm", jvm.value),
      timeout = Some(SingleModuleBuildTool.Mill.defaultTimeout)
    )(files: _*) {
      (workspace, server, positions, osOpt) =>
        SingleModuleBuildTool.Mill.millSetup(
          workspace,
          osOpt,
          readOnlyToplevelSymbolsCache = false
        )

        for (i <- (1 to 4).iterator ++ (11 to 14).iterator) {
          val hover = hoverMarkdown(
            server,
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

}
