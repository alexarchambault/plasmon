package plasmon.pc

import org.eclipse.lsp4j as l

import java.util.concurrent.{ExecutionException, TimeUnit}

import scala.jdk.CollectionConverters.*
import scala.meta.internal.metals.CompilerOffsetParams
import scala.meta.internal.mtags.GlobalSymbolIndex

/** Runs real Scala 2 presentation compiler hosts, whose class paths the build passes us */
class RemotePcHostTests extends munit.FunSuite {

  override def munitTimeout = scala.concurrent.duration.Duration(2, "min")

  private def javaHome = os.Path(sys.props("java.home"))

  private def scalaLibrary(scalaVersion: String): os.Path = {
    val files = coursierapi.Fetch.create()
      .addDependencies(
        coursierapi.Dependency.of("org.scala-lang", "scala-library", scalaVersion)
          .withTransitive(false)
      )
      .fetch()
      .asScala
      .toSeq
    os.Path(files.head)
  }

  private def withHost[T](scalaBinaryVersion: String)(f: RemotePcHost => T): T = {
    val host = RemotePcHost.scala2Hosts(javaHome)(scalaBinaryVersion)
    try f(host)
    finally host.close()
  }

  private def newPc(host: RemotePcHost): RemotePresentationCompiler = {
    val pc = new RemotePresentationCompiler(
      host,
      javaHome,
      () => _ => (),
      GlobalSymbolIndex.BuildTarget("file:///test/?id=test")
    )
    pc.newInstance(
      "buildTarget:file:///test/?id=test",
      Seq(scalaLibrary(host.scalaVersion), javaHome / "jmods/java.base.jmod").map(_.toNIO).asJava,
      Nil.asJava
    )
    pc
  }

  private def hoverParams = {
    val code = "package a\nobject A { val n = List(1, 2).head }\n"
    CompilerOffsetParams(
      java.net.URI.create("file:///test/A.scala"),
      code,
      code.indexOf(".head") + 2
    )
  }

  private def hoverText(pc: RemotePresentationCompiler): String = {
    val hover = pc.hover(hoverParams).get(1, TimeUnit.MINUTES)
    assert(hover.isPresent)
    hover.get.toLsp().getContents.getRight.asInstanceOf[l.MarkupContent].getValue
  }

  for ((scalaBinaryVersion, scalaVersion) <- RemotePcHost.scala2Versions.toSeq.sorted)
    test(s"hover with Scala $scalaBinaryVersion") {
      withHost(scalaBinaryVersion) { host =>
        val pc = newPc(host)
        assert(hoverText(pc).contains("def head: Int"))
        assert(pc.isLoaded())
        assertEquals(pc.scalaVersion(), scalaVersion)
        assert(pc.supportedCodeActions().asScala.contains("ExtractMethod"))
      }
    }

  test("host gets started again after it exits") {
    withHost("2.13") { host =>
      val pc = newPc(host)
      assert(hoverText(pc).contains("def head: Int"))

      val conn     = host.connection()
      val inFlight = pc.hover(hoverParams)
      conn.destroy()
      // Either answered before the host went away, or failed: shouldn't hang in any case
      try inFlight.get(1, TimeUnit.MINUTES)
      catch {
        case _: ExecutionException =>
      }
      assert(!conn.isAlive)
      assert(!pc.isLoaded())

      assert(hoverText(pc).contains("def head: Int"))
      assert(host.connection() ne conn)
    }
  }
}
