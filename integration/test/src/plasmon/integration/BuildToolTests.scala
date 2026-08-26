package plasmon.integration

import plasmon.integration.TestUtil.*

/** Checks what the build tools tell us about a project.
  *
  * One project per build tool, each exercising the features we care about - several modules, a
  * dependency between them, third-party dependencies, mixed Java and Scala sources, and a test
  * module. The assertion is on the serialized BSP data alone: no presentation compiler work happens
  * here, that lives in the suites that replay these recordings.
  *
  * These are the slow tests. They are the only ones that run Mill, sbt and Scala CLI for real, and
  * they are what stands behind the recordings the presentation compiler tests are fed - if a build
  * tool starts reporting something different, it shows up here.
  */
class BuildToolTests extends PlasmonSuite {

  for ((buildTool, jvm, testNameSuffix) <- buildToolJvmValues)
    test("build data" + testNameSuffix) {
      buildDataTest(buildTool, jvm)
    }

  private def scalaVersion = IntegrationConstants.scala3

  private def buildDataTest(
    buildTool: SingleModuleBuildTool,
    jvm: Labelled[String]
  ): Unit = {

    val files = BuildToolTests.projectFiles(buildTool, scalaVersion, jvm.value)

    withWorkspaceAndServer(
      extraServerOpts = Seq("--jvm", jvm.value),
      timeout = Some(buildTool.defaultTimeout * 2)
    )(files.map { case (path, content) => (path, content: os.Source) }*) {
      (workspace, remoteServer, _, osOpt, _) =>

        buildTool.setup(workspace, remoteServer, osOpt, compiles = false)

        BspDataFixture.check(
          workspace,
          os.sub / "build-tool-tests" / buildTool.id / s"jvm-${jvm.label}",
          osOpt
        )
    }
  }
}

object BuildToolTests {

  private def coreScala =
    """package core
      |
      |import com.google.common.base.Strings
      |
      |case class Greeting(name: String) {
      |  def render: String = Strings.padEnd(s"hello, $name", 20, '!')
      |}
      |""".stripMargin

  private def coreJava =
    """package core;
      |
      |public final class Ids {
      |  public static String prefixed(String value) {
      |    return "id-" + value;
      |  }
      |}
      |""".stripMargin

  private def appScala =
    """package app
      |
      |import core.{Greeting, Ids}
      |
      |object Main {
      |  def main(args: Array[String]): Unit =
      |    println(Ids.prefixed(Greeting("world").render))
      |}
      |""".stripMargin

  private def appTestScala =
    """package app
      |
      |import core.Greeting
      |
      |class MainTests extends munit.FunSuite {
      |  test("greeting") {
      |    assert(Greeting("world").render.startsWith("hello"))
      |  }
      |}
      |""".stripMargin

  private def guava    = "com.google.guava:guava:33.2.1-jre"
  private def munitDep = "org.scalameta::munit:1.0.2"

  /** The same project, expressed the way each build tool expects it. */
  def projectFiles(
    buildTool: SingleModuleBuildTool,
    scalaVersion: String,
    jvm: String
  ): Seq[(os.SubPath, String)] =
    buildTool match {
      case SingleModuleBuildTool.Mill =>
        Seq(
          os.sub / "build.mill" ->
            s"""import mill._
               |import mill.scalalib._
               |
               |object core extends ScalaModule {
               |  def jvmId = "$jvm"
               |  def scalaVersion = "$scalaVersion"
               |  def mvnDeps = Seq(mvn"$guava")
               |}
               |
               |object app extends ScalaModule {
               |  def jvmId = "$jvm"
               |  def scalaVersion = "$scalaVersion"
               |  def moduleDeps = Seq(core)
               |
               |  object test extends ScalaTests {
               |    def mvnDeps = Seq(mvn"$munitDep")
               |    def testFramework = "munit.Framework"
               |  }
               |}
               |""".stripMargin,
          (os.sub / "core/src/core/Greeting.scala")     -> coreScala,
          (os.sub / "core/src/core/Ids.java")           -> coreJava,
          (os.sub / "app/src/app/Main.scala")           -> appScala,
          (os.sub / "app/test/src/app/MainTests.scala") -> appTestScala
        )

      case SingleModuleBuildTool.Sbt =>
        Seq(
          os.sub / "build.sbt" ->
            s"""lazy val core = project.settings(
               |  scalaVersion := "$scalaVersion",
               |  libraryDependencies += "com.google.guava" % "guava" % "33.2.1-jre"
               |)
               |
               |lazy val app = project
               |  .dependsOn(core)
               |  .settings(
               |    scalaVersion := "$scalaVersion",
               |    libraryDependencies += "org.scalameta" %% "munit" % "1.0.2" % Test
               |  )
               |""".stripMargin,
          (os.sub / "core/src/main/scala/core/Greeting.scala") -> coreScala,
          (os.sub / "core/src/main/java/core/Ids.java")        -> coreJava,
          (os.sub / "app/src/main/scala/app/Main.scala")       -> appScala,
          (os.sub / "app/src/test/scala/app/MainTests.scala")  -> appTestScala
        )

      case _: SingleModuleBuildTool.ScalaCli =>
        // Scala CLI is single-module, so the modules collapse into one flat set of sources at the
        // workspace root - which is also where it looks for the directives being exercised here
        Seq(
          (os.sub / "Greeting.scala") ->
            s"""//> using scala $scalaVersion
               |//> using jvm $jvm
               |//> using dep $guava
               |$coreScala""".stripMargin,
          (os.sub / "Ids.java")   -> coreJava,
          (os.sub / "Main.scala") -> appScala
        )

      case other =>
        sys.error(s"No build tool test project defined for $other")
    }
}
