package plasmon.command

import com.google.gson.JsonElement
import plasmon.protocol.{Command as ProtocolCommand, CommandClient, CommandResult, PrintData}

import java.net.{ServerSocket, URI}
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.nio.file.Files
import java.util.concurrent.ConcurrentLinkedQueue

import scala.concurrent.duration.DurationInt
import scala.jdk.CollectionConverters.*
import scala.util.Try

/** The web server of `plasmon browse`, against a fake plasmon server
  *
  * The commands that the page's LSP requests go through are answered by [[BrowseAppTests.Fake]],
  * that records them. What the actual commands answer is up to the integration tests.
  */
class BrowseAppTests extends munit.FunSuite {
  import BrowseAppTests.*

  private val http = HttpClient.newBuilder()
    .followRedirects(HttpClient.Redirect.NEVER)
    .build()

  private final class Running(
    val root: os.Path,
    val port: Int,
    val fake: Fake,
    val browseStatus: BrowseStatus,
    val focused: ConcurrentLinkedQueue[os.Path]
  ) {
    def get(pathAndQuery: String): HttpResponse[String] =
      http.send(
        HttpRequest.newBuilder(URI.create(s"http://127.0.0.1:$port$pathAndQuery")).build(),
        HttpResponse.BodyHandlers.ofString()
      )
    def ok(pathAndQuery: String): String = {
      val res = get(pathAndQuery)
      assertEquals(res.statusCode(), 200, s"GET $pathAndQuery: ${res.body()}")
      res.body()
    }
    def json(pathAndQuery: String): ujson.Value =
      ujson.read(ok(pathAndQuery))
    def status(): ujson.Value =
      json("/api/status?since=-1")

    /** Waits for the status bar to show `text` */
    def awaitStatus(text: String): ujson.Value = {
      val deadline = System.currentTimeMillis() + 20.seconds.toMillis
      var res      = status()
      while (res("text").str != text && System.currentTimeMillis() < deadline) {
        Thread.sleep(20L)
        res = status()
      }
      assertEquals(res("text").str, text)
      res
    }
  }

  /** Starts a browse server on a temporary directory, created with `setup` */
  private def withApp[T](
    setup: os.Path => Unit = _ => (),
    reply: Call => Reply = _ => Reply()
  )(f: Running => T): T = {
    val dir = os.temp.dir(prefix = "plasmon-browse-tests")
    try {
      val root = dir / "project"
      os.makeDir(root)
      setup(root)
      val realRoot = root.toNIO.toRealPath()
      val fake     = new Fake(reply)
      val port     = freePort()
      val status   = new BrowseStatus(os.Path(realRoot))
      val focused  = new ConcurrentLinkedQueue[os.Path]
      val app =
        new BrowseApp(realRoot, "127.0.0.1", port, None, fake.inProcess, status, focused.add(_))
      val stop = app.start()
      try f(new Running(os.Path(realRoot), port, fake, status, focused))
      finally stop()
    }
    finally os.remove.all(dir)
  }

  test("the page, at the root and under a prefix") {
    withApp() { app =>
      for (path <- Seq("/", "/index.html", "/some/prefix/", "/some/prefix/index.html")) {
        val res = app.get(path)
        assertEquals(res.statusCode(), 200, path)
        assert(res.headers().firstValue("Content-Type").orElse("").startsWith("text/html"))
        assert(res.body().contains("<title>project</title>"), path)
      }
    }
  }

  test("redirects to a trailing slash, relatively to keep a prefix") {
    withApp() { app =>
      def location(path: String) = {
        val res = app.get(path)
        assertEquals(res.statusCode(), 302, path)
        res.headers().firstValue("Location").orElse("")
      }
      assertEquals(location("/some/prefix"), "prefix/")
      assertEquals(location("/browse"), "browse/")
      assertEquals(location("/a%20b"), "a%20b/")
    }
  }

  test("tree") {
    withApp(
      setup = root => {
        os.write(root / "b.scala", "")
        os.write(root / "A.txt", "")
        os.write(root / "c.sc", "")
        os.makeDir(root / "zdir")
        os.makeDir(root / "Bdir")
        os.write(root / "adir" / "nested.mill", "", createFolders = true)
        os.write(
          root / ".gitattributes",
          "*.sc linguist-language=Scala\nadir/*.mill linguist-language=Mill\n"
        )
        os.write(root / os.up / "other" / "x.txt", "", createFolders = true)
      }
    ) { app =>
      val entries = app.json("/api/tree?path=").arr.toSeq
      assertEquals(
        entries.map(e => (e("name").str, e("path").str, e("dir").bool)),
        Seq(
          ("adir", "adir", true),
          ("Bdir", "Bdir", true),
          ("zdir", "zdir", true),
          (".gitattributes", ".gitattributes", false),
          ("A.txt", "A.txt", false),
          ("b.scala", "b.scala", false),
          ("c.sc", "c.sc", false)
        )
      )
      assertEquals(
        entries.map(e => e("name").str -> e.obj.get("language").map(_.str)).toMap
          .collect { case (k, Some(v)) => k -> v },
        Map("c.sc" -> "Scala")
      )
      assert(entries.forall(e => !e.obj.contains("ignored")))

      // Sub-directories, under a prefix too
      for (prefix <- Seq("", "/some/prefix")) {
        val nested = app.json(s"$prefix/api/tree?path=adir").arr.toSeq
        assertEquals(nested.map(_("path").str), Seq("adir/nested.mill"))
        assertEquals(nested.map(_("language").str), Seq("Mill"))
      }
      // Normalized, as long as it stays under the root
      assertEquals(
        app.json("/api/tree?path=zdir/../adir").arr.map(_("path").str).toSeq,
        Seq("adir/nested.mill")
      )
      // Extra parameters are ignored
      assertEquals(app.json("/api/tree?path=adir&foo=bar").arr.length, 1)
      assertEquals(app.json("/api/tree").arr.length, entries.length)
      assertEquals(app.json("/api/tree?path=zdir").arr.length, 0)

      // Out and back in is fine
      assertEquals(app.json("/api/tree?path=../project/adir").arr.length, 1)
      for (path <- Seq("nope", "b.scala", "..", "../other", "/", "adir/../.."))
        assertEquals(
          app.get(s"/api/tree?path=${enc(path)}").statusCode(),
          404,
          s"path: $path"
        )
    }
  }

  test("tree marks what Git ignores") {
    assume(Try(os.proc("git", "--version").call()).isSuccess, "git not available")
    withApp(
      setup = root => {
        os.proc("git", "init", "-q").call(cwd = root)
        os.write(root / ".gitignore", "*.log\nbuild/\n")
        os.write(root / "a.log", "")
        os.write(root / "a.scala", "")
        os.write(root / "build" / "out.txt", "", createFolders = true)
        os.write(root / "src" / "b.log", "", createFolders = true)
        os.write(root / "src" / "b.scala", "")
      }
    ) { app =>
      def ignored(path: String) =
        app.json(s"/api/tree?path=$path").arr.toSeq
          .filter(_.obj.get("ignored").exists(_.bool))
          .map(_("name").str)
          .toSet
      assertEquals(ignored(""), Set(".git", "a.log", "build"))
      assertEquals(ignored("src"), Set("b.log"))
      // The content of ignored directories is ignored too
      assertEquals(ignored("build"), Set("out.txt"))
    }
  }

  test("gitIgnored outside of a repository") {
    val dir = os.temp.dir(prefix = "plasmon-browse-tests")
    try {
      // Unless the temporary directory is in a repository itself
      val inRepo = Try(
        os.proc("git", "rev-parse", "--git-dir").call(cwd = dir, stderr = os.Pipe)
      ).isSuccess
      assume(!inRepo, s"$dir is in a Git repository")
      // git exits without reading its input then, which mustn't kill a thread writing it
      // (that printed its stack trace)
      val uncaught = new ConcurrentLinkedQueue[Throwable]
      val former   = Thread.getDefaultUncaughtExceptionHandler
      Thread.setDefaultUncaughtExceptionHandler((_, e) => uncaught.add(e))
      try {
        // More than a pipe buffer holds, for writing to block until git is gone
        val names = (1 to 10000).map(i => s"some-file-$i.log")
        assertEquals(BrowseApp.gitIgnored(dir.toNIO, names :+ ".git"), Set(".git"))
        assertEquals(BrowseApp.gitIgnored(dir.toNIO, Nil), Set.empty[String])
        Thread.sleep(200L)
      }
      finally Thread.setDefaultUncaughtExceptionHandler(former)
      assertEquals(uncaught.asScala.toList, Nil)
    }
    finally os.remove.all(dir)
  }

  test("file") {
    withApp(
      setup = root => {
        os.write(root / "Foo.scala", "object Foo // é\n")
        os.write(root / "build.sc", "// build\n")
        os.write(root / ".gitattributes", "*.sc linguist-language=Scala\n")
        os.write(root / "bin.dat", Array[Byte](1, 2, 0, 3))
        os.write(root / "dir" / "a.txt", "a", createFolders = true)
        os.write(root / "large.txt", Array.fill[Byte](10 * 1024 * 1024 + 1)('a'.toByte))
        os.write(root / os.up / "other" / "x.txt", "", createFolders = true)
      }
    ) { app =>
      val foo = app.json("/api/file?path=Foo.scala")
      assertEquals(foo("content").str, "object Foo // é\n")
      assertEquals(foo("size").num, "object Foo // é\n".getBytes("UTF-8").length.toDouble)
      assert(!foo.obj.contains("language"))
      assert(!foo.obj.contains("binary"))

      val build = app.json("/some/prefix/api/file?path=build.sc")
      assertEquals(build("content").str, "// build\n")
      assertEquals(build("language").str, "Scala")

      assertEquals(app.json("/api/file?path=dir/a.txt")("content").str, "a")

      val bin = app.json("/api/file?path=bin.dat")
      assertEquals(bin("binary").bool, true)
      assert(!bin.obj.contains("content"))

      val large = app.json("/api/file?path=large.txt")
      assertEquals(large("tooLarge").bool, true)
      assertEquals(large("size").num, (10 * 1024 * 1024 + 1).toDouble)
      assert(!large.obj.contains("content"))

      assertEquals(app.json("/api/file?path=../project/dir/a.txt")("content").str, "a")
      for (path <- Seq("", "nope", "dir", "../other/x.txt", "/etc/passwd", "dir/../../other/x.txt"))
        assertEquals(
          app.get(s"/api/file?path=${enc(path)}").statusCode(),
          404,
          s"path: $path"
        )
    }
  }

  test("symbolic links stay under the root") {
    val outside = os.temp.dir(prefix = "plasmon-browse-tests-outside")
    try {
      os.write(outside / "secret.txt", "secret")
      withApp(
        setup = root => {
          os.write(root / "dir" / "a.txt", "a", createFolders = true)
          Files.createSymbolicLink((root / "out-file").toNIO, (outside / "secret.txt").toNIO)
          Files.createSymbolicLink((root / "out-dir").toNIO, outside.toNIO)
          Files.createSymbolicLink((root / "in-file").toNIO, (root / "dir" / "a.txt").toNIO)
          Files.createSymbolicLink((root / "in-dir").toNIO, (root / "dir").toNIO)
        }
      ) { app =>
        assertEquals(app.get("/api/file?path=out-file").statusCode(), 404)
        assertEquals(app.get("/api/file?path=out-dir/secret.txt").statusCode(), 404)
        assertEquals(app.get("/api/tree?path=out-dir").statusCode(), 404)
        assertEquals(app.json("/api/file?path=in-file")("content").str, "a")
        assertEquals(app.json("/api/file?path=in-dir/a.txt")("content").str, "a")
        // Under the link, so that the page shows them there
        assertEquals(
          app.json("/api/tree?path=in-dir").arr.map(_("path").str).toSeq,
          Seq("in-dir/a.txt")
        )
        assertEquals(
          app.json("/api/tree?path=").arr.map(_("path").str).toSeq,
          Seq("dir", "in-dir", "out-dir", "in-file", "out-file")
        )
      }
    }
    finally os.remove.all(outside)
  }

  test("load what a file needs, once") {
    withApp(
      setup = root => os.write(root / "src" / "Foo.scala", "object Foo\n", createFolders = true),
      reply = {
        case call if call.name == Seq("lsp", "code-lens") =>
          Reply(stderr = Seq(s"Looking at ${call.args.head}", "Loaded module foo"))
        case _ => Reply()
      }
    ) { app =>
      assertEquals(app.json("/api/load?path=src/Foo.scala"), ujson.Obj())
      val status = app.awaitStatus("src/Foo.scala: ready (module foo)")
      assertEquals(status("busy").bool, false)
      assertEquals(status("error").bool, false)
      // Paths are shown relative to the root
      assertEquals(
        status("lines").arr.map(_.str).toSeq,
        Seq(
          "Loading build tool and module for src/Foo.scala…",
          "Looking at src/Foo.scala",
          "Loaded module foo",
          "src/Foo.scala: ready (module foo)"
        )
      )

      assertEquals(app.json("/api/load?path=src/Foo.scala"), ujson.Obj())
      val calls = app.fake.calls.asScala.toSeq
      assertEquals(calls.length, 1)
      assertEquals(calls.head.name, Seq("lsp", "code-lens"))
      assertEquals(calls.head.args, Seq((app.root / "src" / "Foo.scala").toString))
      assertEquals(calls.head.options("auto").bool, true)
      assertEquals(calls.head.options("json").bool, true)

      assertEquals(app.get("/api/load?path=nope.scala").statusCode(), 404)
      assertEquals(app.get("/api/load?path=src").statusCode(), 404)
    }
  }

  test("focus, when loading and on its own") {
    withApp(
      setup = root => os.write(root / "src" / "Foo.scala", "object Foo\n", createFolders = true)
    ) { app =>
      val foo = app.root / "src" / "Foo.scala"
      app.json("/api/load?path=src/Foo.scala")
      assertEquals(app.json("/api/focus?path=src/Foo.scala"), ujson.Obj())
      assertEquals(app.focused.asScala.toSeq, Seq(foo, foo))
      assertEquals(app.get("/api/focus?path=nope.scala").statusCode(), 404)
      assertEquals(app.get("/api/focus?path=src").statusCode(), 404)
      assertEquals(app.get("/api/focus?path=../outside").statusCode(), 404)
    }
  }

  test("status: the logs of the server, its status items") {
    withApp(
      setup = root => os.write(root / "Foo.scala", "object Foo\n")
    ) { app =>
      import plasmon.languageclient.PlasmonLanguageClient
      app.browseStatus.client.log(
        PlasmonLanguageClient.LogMessage("indexer", "Indexer", java.util.List.of("Indexing"))
      )
      app.browseStatus.client.statusUpdate(
        (app.root / "Foo.scala").toNIO.toUri.toASCIIString,
        java.util.List.of(
          PlasmonLanguageClient.StatusUpdate("plasmon.interactive", "Idle", 0, false)
        )
      )
      app.browseStatus.line("browse line")

      val res = app.json("/api/status?since=-1&log=indexer&fromLine=0")
      assertEquals(res("log").str, "indexer")
      assertEquals(res("lines"), ujson.Arr("Indexing"))
      assertEquals(res("file").str, "Foo.scala")
      assertEquals(res("items").arr.map(_("text").str).toSeq, Seq("Idle"))
      assert(res("channels").arr.exists(_("label").str == "Indexer"))
      // The log of browse by default, none when asked for none
      assertEquals(app.status()("lines"), ujson.Arr("browse line"))
      assert(!app.json("/api/status?since=-1&log=").obj.contains("lines"))
    }
  }

  test("load: module named after the file, dependency sources, failures") {
    withApp(
      setup = root => {
        os.write(root / "a.sc", "")
        os.write(root / "b.scala", "")
        os.write(root / "c.scala", "")
      },
      reply = call =>
        call.args.head.split('/').last match {
          case "a.sc" => Reply(stderr = Seq("Loaded module a.sc"))
          case "b.scala" =>
            Reply(stderr = Seq("b.scala is a dependency source, nothing to load for it"))
          case _ => Reply(exitCode = 1, stderr = Seq("No build tool found"))
        }
    ) { app =>
      app.json("/api/load?path=a.sc")
      app.awaitStatus("a.sc: ready")
      app.json("/api/load?path=b.scala")
      app.awaitStatus("b.scala: dependency source")
      app.json("/api/load?path=c.scala")
      val res = app.awaitStatus("c.scala: No build tool found")
      assertEquals(res("error").bool, true)
    }
  }

  test("hover") {
    val hover = ujson.Obj(
      "contents" -> ujson.Obj(
        "right" -> ujson.Obj("kind" -> "markdown", "value" -> "```scala\ndef foo: Int\n```")
      ),
      "range" -> range(2, 4, 2, 7)
    )
    withApp(
      setup = root => os.write(root / "Foo.scala", "object Foo\n"),
      reply = {
        case call if call.name == Seq("lsp", "hover") =>
          if (call.options("line").num == 0) Reply(stdout = Seq("null"))
          else if (call.options("line").num == 1)
            Reply(exitCode = 1, stderr = Seq("[info] something", "Hover failed"))
          else Reply(stdout = Seq(hover.render()))
        case _ => Reply()
      }
    ) { app =>
      val res = app.json("/api/hover?path=Foo.scala&line=2&col=5")
      assertEquals(res("markdown").str, "```scala\ndef foo: Int\n```")
      assertEquals(res("range"), range(2, 4, 2, 7))

      // What the file needs is loaded first
      val calls = app.fake.calls.asScala.toSeq
      assertEquals(calls.map(_.name), Seq(Seq("lsp", "code-lens"), Seq("lsp", "hover")))
      assertEquals(calls(1).options("line").num, 2.0)
      assertEquals(calls(1).options("col").num, 5.0)
      assertEquals(calls(1).args, Seq((app.root / "Foo.scala").toString))

      assertEquals(
        app.json("/api/hover?path=Foo.scala&line=0&col=0"),
        ujson.Obj("markdown" -> ujson.Null)
      )
      assertEquals(
        app.json("/api/hover?path=Foo.scala&line=1&col=0"),
        ujson.Obj("error" -> "Hover failed")
      )
      // Loaded once
      assertEquals(app.fake.calls.asScala.count(_.name == Seq("lsp", "code-lens")), 1)

      for (
        query <- Seq(
          "path=Foo.scala&line=1",
          "path=Foo.scala&col=1",
          "path=Foo.scala&line=a&col=1",
          "path=Nope.scala&line=1&col=1",
          "line=1&col=1"
        )
      ) {
        assertEquals(app.get(s"/api/hover?$query").statusCode(), 400, query)
        assertEquals(app.get(s"/api/definition?$query").statusCode(), 400, query)
      }
    }
  }

  test("definition") {
    val outside = os.temp.dir(prefix = "plasmon-browse-tests-outside")
    try {
      os.write(outside / "Dep.scala", "")
      withApp(
        setup = root => os.write(root / "a" / "Foo.scala", "object Foo\n", createFolders = true),
        reply = {
          case call if call.name == Seq("lsp", "definition") =>
            val root = os.Path(call.args.head) / os.up / os.up
            val locations = ujson.Arr(
              ujson.Obj(
                "uri"   -> (root / "a" / "Foo.scala").toNIO.toUri.toString,
                "range" -> range(3, 2, 3, 5)
              ),
              ujson.Obj(
                "uri"   -> (outside / "Dep.scala").toNIO.toUri.toString,
                "range" -> range(10, 0, 10, 3)
              ),
              ujson.Obj("uri" -> "jar:file:///x.jar!/a/B.class", "range" -> range(0, 0, 0, 0))
            )
            Reply(stdout = Seq(locations.render()))
          case _ => Reply()
        }
      ) { app =>
        val res = app.json("/api/definition?path=a/Foo.scala&line=1&col=2")("locations").arr.toSeq
        assertEquals(res.map(_("path")), Seq(ujson.Str("a/Foo.scala"), ujson.Null, ujson.Null))
        assertEquals(
          res.map(r => (r("line").num, r("col").num)),
          Seq((3.0, 2.0), (10.0, 0.0), (0.0, 0.0))
        )
        assertEquals(res(1)("uri").str, (outside / "Dep.scala").toNIO.toUri.toString)
      }
    }
    finally os.remove.all(outside)
  }

  test("definition and diagnostics failures") {
    withApp(
      setup = root => os.write(root / "Foo.scala", ""),
      reply = {
        case call if call.name == Seq("lsp", "code-lens") => Reply()
        case _                                            => Reply(exitCode = 2)
      }
    ) { app =>
      assertEquals(
        app.json("/api/definition?path=Foo.scala&line=0&col=0"),
        ujson.Obj("error" -> "Definition failed (exit code 2)")
      )
      assertEquals(
        app.json("/api/diagnostics?path=Foo.scala"),
        ujson.Obj("error" -> "Diagnostics of Foo.scala failed (exit code 2)")
      )
    }
  }

  test("diagnostics") {
    withApp(
      setup = root => os.write(root / "Foo.scala", ""),
      reply = {
        case call if call.name == Seq("lsp", "diagnostics") =>
          val diags = ujson.Arr(
            ujson.Obj(
              "range"    -> range(1, 2, 1, 6),
              "severity" -> "Error",
              "message"  -> ujson.Obj("left" -> "Not found: foo")
            )
          )
          Reply(stdout = Seq(diags.render()), stderr = Seq("Compiling"))
        case _ => Reply()
      }
    ) { app =>
      assertEquals(
        app.json("/api/diagnostics?path=Foo.scala"),
        ujson.Obj(
          "diagnostics" -> ujson.Arr(
            ujson.Obj(
              "line"     -> 1,
              "col"      -> 2,
              "endLine"  -> 1,
              "endCol"   -> 6,
              "message"  -> "Not found: foo",
              "severity" -> "error"
            )
          )
        )
      )
      assertEquals(app.get("/api/diagnostics?path=nope").statusCode(), 404)
      // Shown in the status bar while it runs, logged
      val lines = app.status()("lines").arr.map(_.str).toSeq
      assert(lines.contains("Diagnostics of Foo.scala…"), lines.toString)
      assert(lines.contains("Compiling"), lines.toString)
    }
  }

  test("hover JSON") {
    def markdown(contents: ujson.Value) =
      BrowseApp.hoverJson(ujson.Obj("contents" -> contents))("markdown")

    assertEquals(
      markdown(ujson.Obj("right" -> ujson.Obj("kind" -> "markdown", "value" -> "md"))),
      ujson.Str("md")
    )
    // MarkedString(s), as strings or with a language
    assertEquals(
      markdown(ujson.Obj("left" -> ujson.Arr(ujson.Obj("left" -> "plain")))),
      ujson.Str("plain")
    )
    assertEquals(
      markdown(
        ujson.Obj(
          "left" -> ujson.Arr(
            ujson.Obj("right" -> ujson.Obj("language" -> "scala", "value" -> "def foo: Int")),
            ujson.Obj("left"  -> "Some doc")
          )
        )
      ),
      ujson.Str("```scala\ndef foo: Int\n```\n\n---\n\nSome doc")
    )
    // Without lsp4j's Eithers too
    assertEquals(markdown(ujson.Str("plain")), ujson.Str("plain"))
    assertEquals(markdown(ujson.Obj("kind" -> "plaintext", "value" -> "text")), ujson.Str("text"))
    assertEquals(
      markdown(ujson.Arr("a", ujson.Obj("language" -> "java", "value" -> "int x"))),
      ujson.Str("a\n\n---\n\n```java\nint x\n```")
    )
    assertEquals(markdown(ujson.Num(2)), ujson.Str(""))

    assertEquals(BrowseApp.hoverJson(ujson.Obj("contents" -> "a"))("range"), ujson.Null)
    assertEquals(BrowseApp.hoverJson(ujson.Null), ujson.Obj("markdown" -> ujson.Null))
    assertEquals(BrowseApp.hoverJson(ujson.Obj()), ujson.Obj("markdown" -> ujson.Null))
  }

  test("diagnostics JSON") {
    def diag(message: ujson.Value, severity: Option[String]) = {
      val obj = ujson.Obj("range" -> range(0, 1, 2, 3), "message" -> message)
      for (s <- severity) obj("severity") = s
      obj
    }
    val res = BrowseApp.diagnosticsJson(
      ujson.Arr(
        diag("plain", Some("Warning")),
        diag(ujson.Obj("left" -> "left"), Some("Information")),
        diag(
          ujson.Obj("right" -> ujson.Obj("kind" -> "markdown", "value" -> "right")),
          Some("Hint")
        ),
        diag(ujson.Null, None)
      )
    )
    assertEquals(
      res.arr.map(d => (d("message").str, d("severity").str)).toSeq,
      Seq(("plain", "warning"), ("left", "information"), ("right", "hint"), ("", "error"))
    )
    assertEquals(
      res(0).obj.view.filterKeys(Set("line", "col", "endLine", "endCol")).toMap,
      Map[String, ujson.Value]("line" -> 0, "col" -> 1, "endLine" -> 2, "endCol" -> 3)
    )
    assertEquals(BrowseApp.diagnosticsJson(ujson.Null), ujson.Arr())
  }
}

object BrowseAppTests {

  /** A command run by `plasmon browse`, with its options as JSON */
  final case class Call(name: Seq[String], options: ujson.Value, args: Seq[String])

  /** What a fake command prints and exits with */
  final case class Reply(exitCode: Int = 0, stdout: Seq[String] = Nil, stderr: Seq[String] = Nil)

  /** Answers commands with `reply`, records them in `calls` */
  final class Fake(reply: Call => Reply) {
    val calls = new ConcurrentLinkedQueue[Call]
    val inProcess = new Server.InProcess(
      null,
      (request: ProtocolCommand, client: CommandClient) => {
        def json(elem: JsonElement): ujson.Value = ujson.read(elem.toString)
        val call = Call(
          request.getName.toSeq,
          json(request.getOptions),
          json(request.getRemainingArgs)("remaining").arr.map(_("value").str).toSeq
        )
        calls.add(call)
        val res = reply(call)
        for (line <- res.stderr) client.print(new PrintData(line, true)).get()
        for (line <- res.stdout) client.print(new PrintData(line, false)).get()
        val result = new CommandResult
        result.setExitCode(res.exitCode)
        result
      },
      () => ()
    )
  }

  def range(startLine: Int, startCol: Int, endLine: Int, endCol: Int): ujson.Obj =
    ujson.Obj(
      "start" -> ujson.Obj("line" -> startLine, "character" -> startCol),
      "end"   -> ujson.Obj("line" -> endLine, "character" -> endCol)
    )

  def enc(s: String): String =
    java.net.URLEncoder.encode(s, "UTF-8")

  def freePort(): Int = {
    val socket = new ServerSocket(0)
    try socket.getLocalPort
    finally socket.close()
  }
}
