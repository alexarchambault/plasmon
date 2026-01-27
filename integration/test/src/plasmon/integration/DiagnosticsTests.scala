package plasmon.integration

import io.github.alexarchambault.testutil.TestUtil.*
import org.eclipse.{lsp4j => l}
import plasmon.integration.TestUtil.*

import java.net.URI
import java.nio.file.Paths

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Promise}
import scala.reflect.Selectable.reflectiveSelectable

class DiagnosticsTests extends PlasmonSuite {

  for (jvm <- jvmValues)
    test(s"simple Java ${jvm.label}") {
      simpleTest(SingleModuleBuildTool.ScalaCli, (IntegrationConstants.scala213, "2.13"), jvm)
    }

  private def simpleTest(
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
    val diagPromise = Promise[l.PublishDiagnosticsParams]()
    val futureDiag  = diagPromise.future
    val client
      : MockLanguageClient { def setWorkspace(workspace: os.Path): Unit; def workspace: os.Path } =
      new MockLanguageClient {
        private var workspaceOpt = Option.empty[os.Path]
        def setWorkspace(workspace: os.Path): Unit = {
          workspaceOpt = Some(workspace)
        }
        def workspace = workspaceOpt.getOrElse(sys.error("workspace not set"))
        override def publishDiagnostics(params: l.PublishDiagnosticsParams): Unit = {
          outputStream.pprint(params)
          val path = os.Path(Paths.get(new URI(params.getUri)))
          if (path == workspace / sourceFile && !params.getDiagnostics.isEmpty)
            diagPromise.trySuccess(params)
          super.publishDiagnostics(params)
        }
      }
    withWorkspaceServerPositions(
      client = client,
      extraServerOpts = Seq("--jvm", jvm.value),
      timeout = Some(buildTool.defaultTimeout)
    )(files*) {
      (workspace, _, _, osOpt) =>

        client.setWorkspace(workspace)

        buildTool.setup(workspace, osOpt, compiles = false)

        val diagParams = Await.result(futureDiag, 20.seconds)

        checkGsonFixture(
          fixtureDir / "plasmon/integration/diagnostics-tests/simple" / buildTool.id / s"scala-${scalaVersion._2}" / s"jvm-${jvm.label}" / "publish-diagnostics-params.json",
          diagParams,
          osOpt,
          replaceAll = standardReplacements(workspace)
        )
    }
  }

}
