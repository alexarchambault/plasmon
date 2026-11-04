package plasmon.ide

import ch.epfl.scala.bsp4j as b
import plasmon.bsp.PlasmonBuildServer
import plasmon.index.{BspData, TargetData}

import java.lang.reflect.{InvocationHandler, Proxy}
import java.util.concurrent.{
  CancellationException,
  CompletableFuture,
  LinkedBlockingQueue,
  TimeUnit
}

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*
import scala.util.{Failure, Try}

class CompilationsTests extends munit.FunSuite {

  private given ExecutionContext = ExecutionContext.global

  private final case class CompileRequest(
    targets: Seq[b.BuildTargetIdentifier],
    result: CompletableFuture[b.CompileResult]
  )

  /** A build server whose compilations complete when the test says so */
  private def fakeBuildServer(requests: LinkedBlockingQueue[CompileRequest]): PlasmonBuildServer = {
    lazy val server: PlasmonBuildServer = Proxy
      .newProxyInstance(
        getClass.getClassLoader,
        Array(classOf[PlasmonBuildServer]),
        handler: InvocationHandler
      )
      .asInstanceOf[PlasmonBuildServer]
    lazy val handler: InvocationHandler = (_, method, args) =>
      method.getName match {
        case "buildTargetCompile" =>
          val params = args(0).asInstanceOf[b.CompileParams]
          val f      = new CompletableFuture[b.CompileResult]
          requests.add(CompileRequest(params.getTargets.asScala.toSeq, f))
          f
        case "hashCode" => System.identityHashCode(server)
        case "equals"   => args(0).asInstanceOf[AnyRef] eq server
        case "toString" => "fake-build-server"
        case other      => throw new UnsupportedOperationException(other)
      }
    server
  }

  private def target(name: String) =
    new b.BuildTargetIdentifier(s"file:///workspace/$name")

  private def compilations(
    buildServer: PlasmonBuildServer,
    targets: Seq[b.BuildTargetIdentifier],
    afterSuccessfulCompilation: Seq[b.BuildTargetIdentifier] => Unit = _ => ()
  ): Compilations = {
    val targetData = new TargetData
    targetData.buildServerOpt = Some(buildServer)
    for (t <- targets)
      targetData.targetToWorkspace(t) = os.root / "workspace"
    // the build servers aren't needed to find a target's build server
    val bspData = new BspData(os.root / "workspace", null)
    bspData.data = BspData.DataSeq(List(targetData))
    new Compilations(bspData, afterSuccessfulCompilation, () => (), bestEffortEnabled = false)
  }

  private def nextRequest(requests: LinkedBlockingQueue[CompileRequest]): CompileRequest =
    Option(requests.poll(10L, TimeUnit.SECONDS))
      .getOrElse(fail("No compilation request received"))

  private def result[T](f: Future[T]): Try[T] =
    Try(Await.ready(f, 10.seconds)).flatMap(_ => f.value.get)

  test("cancelAll fails running and queued compilations") {
    val requests   = new LinkedBlockingQueue[CompileRequest]
    val (t1, t2)   = (target("a"), target("b"))
    val compiler   = compilations(fakeBuildServer(requests), Seq(t1, t2))
    val running    = compiler.compileTarget(t1)
    val queued     = compiler.compileTarget(t2)
    val runningReq = nextRequest(requests)
    assertEquals(runningReq.targets, Seq(t1))

    compiler.cancelAll()

    for (f <- Seq(running, queued))
      result(f) match {
        case Failure(_: CancellationException) =>
        case other                             => fail(s"Expected a cancellation, got $other")
      }
  }

  test("an exception after a compilation fails it, and the next compilation still runs") {
    val requests = new LinkedBlockingQueue[CompileRequest]
    val (t1, t2) = (target("a"), target("b"))
    val compiler = compilations(
      fakeBuildServer(requests),
      Seq(t1, t2),
      afterSuccessfulCompilation = targets =>
        if (targets.contains(t1)) throw new Exception("after compilation")
    )
    val first  = compiler.compileTarget(t1)
    val second = compiler.compileTarget(t2)

    val compileRes = new b.CompileResult(b.StatusCode.OK)
    nextRequest(requests).result.complete(compileRes)
    result(first) match {
      case Failure(ex) => assertEquals(ex.getMessage, "after compilation")
      case other       => fail(s"Expected a failure, got $other")
    }

    val secondReq = nextRequest(requests)
    assertEquals(secondReq.targets, Seq(t2))
    secondReq.result.complete(compileRes)
    assertEquals(result(second).get, compileRes)
  }
}
