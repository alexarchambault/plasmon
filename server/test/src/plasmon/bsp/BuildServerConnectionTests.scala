package plasmon.bsp

import ch.epfl.scala.bsp4j as b
import org.eclipse.lsp4j.jsonrpc.Launcher
import org.eclipse.lsp4j.jsonrpc.services.{JsonNotification, JsonRequest}
import plasmon.TestLogger
import plasmon.ide.Buffers
import plasmon.languageclient.PlasmonNoopLanguageClient
import plasmon.servercommand.BspUtil

import java.io.{PipedInputStream, PipedOutputStream}
import java.util.concurrent.{CompletableFuture, ExecutionException, Executors, TimeUnit}

import scala.jdk.CollectionConverters.*

class BuildServerConnectionTests extends munit.FunSuite {

  /** Answers the initialization handshake, and receives compilation requests without answering them
    */
  class FakeBuildServer {
    val compileReceived = new CompletableFuture[Unit]

    @JsonRequest("build/initialize")
    def initialize(params: b.InitializeBuildParams): CompletableFuture[b.InitializeBuildResult] =
      CompletableFuture.completedFuture(
        new b.InitializeBuildResult(
          "fake",
          "0.1.0",
          b.Bsp4j.PROTOCOL_VERSION,
          new b.BuildServerCapabilities
        )
      )
    @JsonNotification("build/initialized")
    def initialized(): Unit = ()

    @JsonRequest("buildTarget/compile")
    def compile(params: b.CompileParams): CompletableFuture[b.CompileResult] = {
      compileReceived.complete(())
      new CompletableFuture
    }
  }

  test("pending and later requests fail when the build server goes away") {
    val workspace = os.temp.dir(prefix = "plasmon-test")
    val pool      = Executors.newCachedThreadPool()
    try {
      val toServer   = new PipedInputStream(1 << 16)
      val fromClient = new PipedOutputStream(toServer)
      val toClient   = new PipedInputStream(1 << 16)
      val fromServer = new PipedOutputStream(toClient)

      val fakeServer = new FakeBuildServer
      new Launcher.Builder[b.BuildClient]()
        .setExecutorService(pool)
        .setInput(toServer)
        .setOutput(fromServer)
        .setLocalService(fakeServer)
        .setRemoteInterface(classOf[b.BuildClient])
        .create()
        .startListening()

      val buildToolId = "fake"
      val (buildServer, _, _, _) = BspUtil.bspServerFromStreams(
        toClient,
        fromClient,
        workspace,
        _ => (),
        () =>
          new PlasmonBuildClientImpl(
            PlasmonNoopLanguageClient,
            Buffers(),
            // only needed when the build server publishes diagnostics, which this one doesn't
            null,
            _ => (),
            buildToolId,
            "Fake"
          ),
        PlasmonNoopLanguageClient,
        buildToolId,
        "Fake",
        pool,
        logJsonrpcInput = false,
        initData = null,
        fakeMetals = false,
        logger = TestLogger
      )

      val target  = new b.BuildTargetIdentifier(workspace.toNIO.toUri.toASCIIString)
      val compile = buildServer.buildTargetCompile(new b.CompileParams(List(target).asJava))
      fakeServer.compileReceived.get(10L, TimeUnit.SECONDS)

      // the build server goes away without answering
      fromServer.close()
      toServer.close()

      def checkFailed(f: CompletableFuture[?]): Unit = {
        val ex = intercept[ExecutionException](f.get(10L, TimeUnit.SECONDS))
        assert(
          ex.getCause.isInstanceOf[BuildServerConnectionClosedException],
          s"Unexpected exception: ${ex.getCause}"
        )
      }

      checkFailed(compile)
      checkFailed(buildServer.buildTargetCompile(new b.CompileParams(List(target).asJava)))
    }
    finally {
      pool.shutdownNow()
      os.remove.all(workspace)
    }
  }
}
