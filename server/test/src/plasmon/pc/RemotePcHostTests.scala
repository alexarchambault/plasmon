package plasmon.pc

import org.eclipse.lsp4j as l

import java.util.concurrent.{ExecutionException, TimeUnit}

import scala.jdk.CollectionConverters.*
import scala.meta.internal.metals.CompilerOffsetParams
import scala.meta.internal.mtags.GlobalSymbolIndex

/** Runs a real Scala 2 presentation compiler host, whose class path the build passes us */
class RemotePcHostTests extends munit.FunSuite {

  override def munitTimeout = scala.concurrent.duration.Duration(2, "min")

  private def javaHome = os.Path(sys.props("java.home"))

  private lazy val scala213Library: os.Path = {
    val files = coursierapi.Fetch.create()
      .addDependencies(
        coursierapi.Dependency.of(
          "org.scala-lang",
          "scala-library",
          plasmon.internal.Constants.scala2Version
        ).withTransitive(false)
      )
      .fetch()
      .asScala
      .toSeq
    os.Path(files.head)
  }

  private def withHost[T](f: RemotePcHost => T): T = {
    val host = new RemotePcHost(
      "Scala 2",
      () => RemotePcHost.scala2ClassPath(),
      RemotePcHost.scala2MainClass,
      javaHome
    )
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
      Seq(scala213Library, javaHome / "jmods/java.base.jmod").map(_.toNIO).asJava,
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

  test("hover") {
    withHost { host =>
      val pc = newPc(host)
      assert(hoverText(pc).contains("def head: Int"))
      assert(pc.isLoaded())
      assert(pc.scalaVersion().startsWith("2.13."))
      assert(pc.supportedCodeActions().asScala.contains("ExtractMethod"))
    }
  }

  test("host gets started again after it exits") {
    withHost { host =>
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
