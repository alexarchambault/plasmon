package plasmon.command

import org.eclipse.lsp4j as l
import plasmon.languageclient.PlasmonLanguageClient

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.jdk.CollectionConverters.*

/** The status bar and logs of `plasmon browse`, that its page long-polls */
class BrowseStatusTests extends munit.FunSuite {

  private def lines(obj: ujson.Obj): Seq[String] =
    obj("lines").arr.map(_.str).toSeq

  test("busy, then back to what's shown when idle") {
    val status = new BrowseStatus
    val start  = status.await(-1L, 0L, 0L)
    assertEquals(start("text").str, "Ready")
    assertEquals(start("busy").bool, false)
    assertEquals(lines(start), Nil)

    status.busy("Loading things")
    val busy = status.await(-1L, 0L, 0L)
    assertEquals(busy("text").str, "Loading things…")
    assertEquals(busy("busy").bool, true)
    assert(busy("seq").num > start("seq").num)

    // Progress goes to the status bar, the logs of requests (`[…] …`) only to the log
    status.line("Compiling")
    status.line("[info] chatty")
    status.line("not for the status bar", inStatusBar = false)
    assertEquals(status.await(-1L, 0L, 0L)("text").str, "Compiling")

    // What to show once done, set while busy
    status.idle("Foo.scala: ready")
    assertEquals(status.await(-1L, 0L, 0L)("text").str, "Compiling")

    status.done()
    val done = status.await(-1L, 0L, 0L)
    assertEquals(done("text").str, "Foo.scala: ready")
    assertEquals(done("busy").bool, false)
    assertEquals(done("error").bool, false)
    assertEquals(
      lines(done),
      Seq(
        "Loading things…",
        "Compiling",
        "[info] chatty",
        "not for the status bar",
        "Foo.scala: ready"
      )
    )
    // Only the lines that came after
    assertEquals(
      lines(status.await(-1L, 3L, 0L)),
      Seq("not for the status bar", "Foo.scala: ready")
    )
    assertEquals(status.await(-1L, 3L, 0L)("fromLine").num, 3.0)
  }

  test("nested busy states") {
    val status = new BrowseStatus
    status.busy("First")
    status.busy("Second")
    status.done()
    assertEquals(status.await(-1L, 0L, 0L)("busy").bool, true)
    status.done()
    assertEquals(status.await(-1L, 0L, 0L)("busy").bool, false)
    // More done than busy: doesn't go below zero
    status.done()
    status.busy("Third")
    assertEquals(status.await(-1L, 0L, 0L)("busy").bool, true)
  }

  test("errors") {
    val status = new BrowseStatus
    status.idle("Foo.scala: no build tool", error = true)
    val res = status.await(-1L, 0L, 0L)
    assertEquals(res("text").str, "Foo.scala: no build tool")
    assertEquals(res("error").bool, true)
    // Not an error while busy, back to it afterwards
    status.busy("Hover")
    assertEquals(status.await(-1L, 0L, 0L)("error").bool, false)
    status.done()
    assertEquals(status.await(-1L, 0L, 0L)("error").bool, true)
  }

  test("only the last lines are kept") {
    val status = new BrowseStatus
    for (i <- 0 until 1005)
      status.line(s"line $i")
    val res = status.await(-1L, 0L, 0L)
    assertEquals(res("fromLine").num, 5.0)
    assertEquals(lines(res).length, 1000)
    assertEquals(lines(res).head, "line 5")
    assertEquals(lines(res).last, "line 1004")
    assertEquals(lines(status.await(-1L, 1003L, 0L)), Seq("line 1003", "line 1004"))
  }

  test("waits for a change") {
    val status = new BrowseStatus
    val seq    = status.await(-1L, 0L, 0L)("seq").num.toLong

    // Nothing changes: returns once the time is up
    val before    = System.nanoTime()
    val unchanged = status.await(seq, 0L, 200L)
    assert(System.nanoTime() - before >= 200L * 1000000L)
    assertEquals(unchanged("seq").num.toLong, seq)

    // Returns as soon as something changes
    val changed = Future(status.await(seq, 0L, 60000L))
    Thread.sleep(100L)
    assert(!changed.isCompleted)
    status.busy("Something")
    val res = Await.result(changed, 10.seconds)
    assertEquals(res("text").str, "Something…")
    assert(res("seq").num.toLong > seq)
  }

  test("log channels, from the server") {
    val status = new BrowseStatus
    status.line("browse line")
    val client = status.client
    // Empty, when the server creates a logger
    client.log(PlasmonLanguageClient.LogMessage("bsp-1", "Scala CLI", Nil.asJava))
    client.log(PlasmonLanguageClient.LogMessage("bsp-1", "Scala CLI", Seq("a", "b").asJava))
    client.log(PlasmonLanguageClient.LogMessage("indexer", "Indexer", Seq("c").asJava))
    client.showMessage(new l.MessageParams(l.MessageType.Error, "Oops"))
    status.serverLog("from scribe")

    val res = status.await(-1L, 0L, 0L, log = Some("bsp-1"))
    assertEquals(
      res("channels").arr.map(c => (c("id").str, c("label").str, c("end").num.toInt)).toSeq,
      Seq(
        ("browse", "Browse", 1),
        ("server", "Server", 1),
        ("bsp-1", "Scala CLI", 2),
        ("indexer", "Indexer", 1),
        ("messages", "Messages", 1)
      )
    )
    assertEquals(res("log").str, "bsp-1")
    assertEquals(lines(res), Seq("a", "b"))
    assertEquals(lines(status.await(-1L, 1L, 0L, log = Some("bsp-1"))), Seq("b"))
    assertEquals(lines(status.await(-1L, 0L, 0L, log = Some("messages"))), Seq("[error] Oops"))
    assertEquals(lines(status.await(-1L, 0L, 0L, log = Some("server"))), Seq("from scribe"))
    // The log of browse by default
    assertEquals(lines(status.await(-1L, 0L, 0L)), Seq("browse line"))
    // No log
    val noLog = status.await(-1L, 0L, 0L, log = None)
    assert(!noLog.value.contains("lines"))
    assert(!status.await(-1L, 0L, 0L, log = Some("unknown")).value.contains("lines"))
  }

  test("server logs keep their last lines") {
    val status = new BrowseStatus
    for (i <- 0 until 5003)
      status.serverLog(s"line $i")
    val res = status.await(-1L, 0L, 0L, log = Some("server"))
    assertEquals(res("fromLine").num, 3.0)
    assertEquals(lines(res).head, "line 3")
    assertEquals(lines(res).length, 5000)
  }

  test("status items of the focused file") {
    val root   = os.temp.dir(prefix = "plasmon-browse-status")
    val status = new BrowseStatus(root)
    val start  = status.await(-1L, 0L, 0L)
    assertEquals(start("file"), ujson.Null)
    assertEquals(start("items"), ujson.Arr())

    val showLog = PlasmonLanguageClient.Command(
      title = "Show log",
      command = "plasmon.show-log",
      arguments = Seq("indexer").asJava
    )
    val other = PlasmonLanguageClient.Command(title = "Restart", command = "plasmon.restart")
    status.client.statusUpdate(
      (root / "src" / "Foo.scala").toNIO.toUri.toASCIIString,
      Seq(
        PlasmonLanguageClient.StatusUpdate("plasmon.indexer", "Indexing", 1, true, "", showLog),
        PlasmonLanguageClient.StatusUpdate(
          "plasmon.interactive",
          "Idle",
          0,
          false,
          "Scala 3",
          other
        )
      ).asJava
    )
    val res = status.await(start("seq").num.toLong, 0L, 1000L)
    assertEquals(res("file").str, "src/Foo.scala")
    assertEquals(
      res("items"),
      ujson.Arr(
        ujson.Obj(
          "id"       -> "plasmon.indexer",
          "text"     -> "Indexing",
          "severity" -> 1,
          "busy"     -> true,
          "log"      -> "indexer"
        ),
        ujson.Obj(
          "id"       -> "plasmon.interactive",
          "text"     -> "Idle",
          "severity" -> 0,
          "busy"     -> false,
          "detail"   -> "Scala 3"
        )
      )
    )

    // Outside of the root: as is
    status.client.statusUpdate("file:///elsewhere/Foo.scala", Nil.asJava)
    assertEquals(status.await(-1L, 0L, 0L)("file").str, "file:///elsewhere/Foo.scala")
    // Nothing focused
    status.client.statusUpdate("", Nil.asJava)
    assertEquals(status.await(-1L, 0L, 0L)("file"), ujson.Null)
  }

  test("build tool requests in progress") {
    val status  = new BrowseStatus
    val details = PlasmonLanguageClient.ProgressDetails("mill", "Mill", "1", "compile", false, null)
    status.client.progress(details)
    status.client.progress(details.copy(requestId = "2", request = "test"))
    // Progress of a request already there
    status.client.progress(details.copy(progress = 0.5))
    assertEquals(
      status.await(-1L, 0L, 0L)("tasks"),
      ujson.Arr("Mill: compile", "Mill: test")
    )
    status.client.progress(details.asDone)
    assertEquals(status.await(-1L, 0L, 0L)("tasks"), ujson.Arr("Mill: test"))
  }
}
