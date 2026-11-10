package plasmon.remotepc.host

import java.io.{PipedInputStream, PipedOutputStream}
import java.net.URI
import java.nio.file.Paths
import java.util.{Collections, List => JList}
import java.util.concurrent.{CompletableFuture, ConcurrentLinkedQueue, Executors}

import org.eclipse.lsp4j.{Location, MarkupContent}
import org.eclipse.lsp4j.jsonrpc.Launcher
import plasmon.remotepc.{Messages, RemotePcClient, RemotePcServer}

import scala.jdk.CollectionConverters._
import scala.meta.internal.metals.CompilerOffsetParams
import scala.meta.internal.pc.PresentationCompilerConfigImpl
import scala.meta.pc.SymbolSearch

class HostServerTests extends munit.FunSuite {

  /** Answers the host like the server would, with a symbol index that only knows ListBuffer */
  private final class FakeClient extends RemotePcClient {
    var server: RemotePcServer = _
    val logs                   = new ConcurrentLinkedQueue[String]
    val accesses               = new ConcurrentLinkedQueue[String]
    val searches               = new ConcurrentLinkedQueue[String]
    val visits                 = new ConcurrentLinkedQueue[String]
    private val pool = Executors.newCachedThreadPool(Scala2PcHost.daemonThreadFactory("test"))

    def log(params: Messages.LogParams): Unit =
      logs.add(params.message)
    def accessStarted(params: Messages.AccessParams): Unit =
      accesses.add(s"start ${params.name}")
    def accessEnded(params: Messages.AccessParams): Unit =
      accesses.add(s"end ${params.name}")
    def documentation(params: Messages.SearchParams)
      : CompletableFuture[Messages.SymbolDocumentationWire] =
      CompletableFuture.completedFuture(null)
    def definition(params: Messages.SearchParams): CompletableFuture[JList[Location]] =
      CompletableFuture.completedFuture(Collections.emptyList())
    def definitionSourceToplevels(params: Messages.SearchParams)
      : CompletableFuture[JList[String]] =
      CompletableFuture.completedFuture(Collections.emptyList())
    def search(params: Messages.SearchParams): CompletableFuture[String] = {
      searches.add(params.query)
      CompletableFuture.supplyAsync(
        { () =>
          if ("ListBuffer".startsWith(params.query)) {
            val visit = new Messages.VisitParams
            visit.searchId = params.searchId
            visit.kind = Messages.VisitParams.CLASSFILE
            visit.pkg = "scala/collection/mutable/"
            visit.filename = "ListBuffer.class"
            val res = server.searchVisit(visit).get()
            visits.add(s"${visit.pkg}${visit.filename}: ${res.added}")
          }
          SymbolSearch.Result.COMPLETE.name
        },
        pool
      )
    }
    def searchMethods(params: Messages.SearchParams): CompletableFuture[String] =
      CompletableFuture.completedFuture(SymbolSearch.Result.COMPLETE.name)
    def workspaceMemberPriority(params: Messages.SearchParams): CompletableFuture[Integer] =
      CompletableFuture.completedFuture(0)
  }

  private def withHost[T](f: (RemotePcServer, FakeClient) => T): T = {
    val toHost   = new PipedOutputStream
    val hostIn   = new PipedInputStream(toHost, 1 << 16)
    val fromHost = new PipedOutputStream
    val clientIn = new PipedInputStream(fromHost, 1 << 16)

    val host = new HostServer
    val hostLauncher = new Launcher.Builder[RemotePcClient]()
      .setLocalService(host)
      .setRemoteInterface(classOf[RemotePcClient])
      .setInput(hostIn)
      .setOutput(fromHost)
      .create()
    host.setClient(hostLauncher.getRemoteProxy)

    val client = new FakeClient
    val clientLauncher = new Launcher.Builder[RemotePcServer]()
      .setLocalService(client)
      .setRemoteInterface(classOf[RemotePcServer])
      .setInput(clientIn)
      .setOutput(toHost)
      .create()
    client.server = clientLauncher.getRemoteProxy

    val hostListening   = hostLauncher.startListening()
    val clientListening = clientLauncher.startListening()
    try f(client.server, client)
    finally {
      host.shutdownAll()
      hostListening.cancel(true)
      clientListening.cancel(true)
      toHost.close()
      fromHost.close()
    }
  }

  private def scalaLibrary =
    Paths.get(classOf[scala.Option[_]].getProtectionDomain.getCodeSource.getLocation.toURI)

  private def create(server: RemotePcServer, pcId: String): Messages.CreateResult = {
    val params = new Messages.CreateParams
    params.pcId = pcId
    params.module = "buildTarget:file:///test/?id=test"
    params.javaHome = sys.props("java.home")
    params.workspace = Paths.get(".").toAbsolutePath.toString
    // Like the server does, we pass the JDK as JMOD files (the class path doesn't have JDK
    // classes otherwise)
    val jmods = Paths.get(sys.props("java.home")).resolve("jmods").resolve("java.base.jmod")
    params.classPath = Seq(scalaLibrary.toString, jmods.toString).asJava
    params.options = Collections.emptyList()
    params.config = Messages.ConfigWire.from(PresentationCompilerConfigImpl())
    server.create(params).get()
  }

  private def fileParams(code: String): Messages.FileParamsWire = {
    val offset = code.indexOf("@@")
    assert(offset >= 0)
    Messages.FileParamsWire.from(
      CompilerOffsetParams(URI.create("file:///A.scala"), code.replace("@@", ""), offset)
    )
  }

  test("hover") {
    withHost { (server, client) =>
      val created = create(server, "pc-1")
      assert(created.scalaVersion.startsWith("2.13."))
      assert(created.supportedCodeActions.contains("ExtractMethod"))

      val req = new Messages.PcRequest("pc-1")
      req.file = fileParams("object A { val n = List(1, 2).he@@ad }")
      val hover = server.hover(req).get()
      assert(hover != null, client.logs.asScala.mkString("\n"))
      val content = hover.toLsp().getContents.getRight.asInstanceOf[MarkupContent].getValue
      assert(content.contains("def head: Int"), content)

      assert(client.accesses.asScala.exists(_.startsWith("start ")), client.accesses)
      assert(client.logs.asScala.exists(_.contains("Creating new interactive compiler")))
    }
  }

  test("completions use the symbol search of the server") {
    withHost { (server, client) =>
      create(server, "pc-2")
      val req = new Messages.PcRequest("pc-2")
      // (with no package clause, the Scala 2 PC fails to compute where auto-imports go)
      req.file = fileParams("package a\n\nobject A {\n  def f = ListBuf@@\n}\n")
      val items = server.complete(req).get().getItems.asScala.toList
      assert(client.searches.contains("ListBuf"), client.searches)
      val labels = items.map(_.getLabel)
      assert(labels.exists(_.startsWith("ListBuffer")), (labels, client.visits))
      // Not in scope: brought by the search, along with an import
      val listBuffer = items.find(_.getLabel.startsWith("ListBuffer")).get
      assert(
        Option(listBuffer.getAdditionalTextEdits).exists(!_.isEmpty),
        listBuffer
      )
    }
  }

  test("unknown presentation compiler") {
    withHost { (server, _) =>
      val req = new Messages.PcRequest("nope")
      req.file = fileParams("object A { @@ }")
      val ex = intercept[java.util.concurrent.ExecutionException](server.complete(req).get())
      assert(ex.getCause.getMessage.contains("No presentation compiler with id nope"))
    }
  }
}
