package plasmon.integration

import plasmon.integration.TestUtil.*

import scala.concurrent.duration.*

class MillTests extends PlasmonSuite {

  def timeout = 5.minutes

  val files = Seq(
    os.sub / "build.mill" ->
      s"""//| mill-version: ${IntegrationConstants.millVersion}
         |
         |package bu<1>ild
         |
         |import mi<2>ll.*
         |import mi<3>ll.sc<4>alalib.*
         |
         |trait F<5>oo extends Cro<6>ssScalaModule {
         |  def mod<7>uleDeps = S<8>eq(bu<9>ild.th<10>ing())
         |  def mvn<11>Deps = super.mvn<12>Deps() ++ Seq(
         |    m<13>vn"io.get-coursier:coursier_2.13:2.1.25-M23"
         |  )
         |}
         |
         |object f<14>oo extends C<15>ross[F<16>oo]("${IntegrationConstants.scala3}")
         |""".stripMargin,
    os.sub / "thing/package.mill" ->
      s"""package bu<1>ild.th<2>ing
         |
         |import mi<3>ll.*
         |import mi<4>ll.sc<5>alalib.*
         |
         |trait Th<6>ing extends Cr<7>ossScalaModule {
         |  println(bu<16>ild.`da<17>sh-thing`)
         |  pri<21>ntln(bui<18>ld.`da<19>sh-thing`.sca<20>laVersion)
         |  object te<8>st extends Sc<9>alaTests, Te<10>stModule.Ut<11>est {
         |    def ut<12>estVersion = "0.9.5"
         |  }
         |}
         |
         |object `p<13>ackage` extends C<14>ross[T<15>hing]("${IntegrationConstants.scala3}")
         |""".stripMargin,
    os.sub / "dash-thing/package.mill" ->
      s"""package bu<1>ild.`dash-th<2>ing`
         |
         |import mi<3>ll.*
         |import mi<4>ll.sc<5>alalib.*
         |
         |object `pac<6>kage` extends Sc<7>alaModule {
         |  def module<13>Deps = Seq(bu<14>ild.f<15>oo("${IntegrationConstants.scala3}"))
         |  def scala<16>Version = "${IntegrationConstants.scala3}"
         |  object te<8>st extends Sc<9>alaTests, Te<10>stModule.Ut<11>est {
         |    def ut<12>estVersion = "0.9.5"
         |  }
         |}
         |""".stripMargin
  )

  test("build files") {

    withWorkspaceServerPositions(
      extraServerOpts = Seq("--jvm", "17"),
      timeout = Some(timeout)
    )(files*) {
      (workspace, server, positions, osOpt) =>

        os.copy(SingleModuleBuildTool.Mill.millwPath, workspace / "mill")
        os.copy(SingleModuleBuildTool.Mill.millwBatPath, workspace / "mill.bat")
        (workspace / "mill").toIO.setExecutable(true)
        TestUtil.runServerCommand(workspace, osOpt)(
          "build-tool",
          "add",
          ".",
          "--mill"
        )
        TestUtil.runServerCommand(workspace, osOpt)(
          "import",
          "--ignore-toplevel-symbols-errors=false"
        )
        TestUtil.runServerCommand(workspace, osOpt)(
          "bsp",
          "compile"
        )

        def hoverAt(source: os.SubPath, pos: Int): Unit =
          checkTextFixture(
            fixtureDir / "plasmon/integration/mill-tests" / source / s"hover-$pos.txt",
            hoverMarkdown(
              server,
              workspace / source,
              positions.lspPos(source, pos)
            ),
            osOpt
          )

        for (i <- 1 to 16)
          hoverAt(os.sub / "build.mill", i)
        for (i <- 1 to 15)
          hoverAt(os.sub / "thing/package.mill", i)
        for (i <- 1 to 16)
          hoverAt(os.sub / "dash-thing/package.mill", i)

        def goToDefAt(source: os.SubPath, pos: Int): Unit = {
          System.err.println(s"Checking $source at position #$pos")
          checkJsoniterFixture(
            fixtureDir / "plasmon/integration/mill-tests" / source / s"definition-$pos.txt",
            goToDefs(
              server,
              workspace,
              workspace / source,
              positions.lspPos(source, pos)
            ),
            osOpt
          )
        }

        for (i <- 1 to 16)
          goToDefAt(os.sub / "build.mill", i)
        for (i <- 1 to 15)
          goToDefAt(os.sub / "thing/package.mill", i)
        for (i <- 1 to 16)
          goToDefAt(os.sub / "dash-thing/package.mill", i)
    }

  }

  override def munitTimeout: Duration =
    (2 * (1 + retryCount)) * timeout
}
