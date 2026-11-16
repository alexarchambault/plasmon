package plasmon.command

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt

/** The status bar and log of `plasmon browse`, that its page long-polls */
class BrowseLspStatusTests extends munit.FunSuite {

  private def lines(obj: ujson.Obj): Seq[String] =
    obj("lines").arr.map(_.str).toSeq

  test("busy, then back to what's shown when idle") {
    val status = new BrowseLsp.Status
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
    val status = new BrowseLsp.Status
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
    val status = new BrowseLsp.Status
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
    val status = new BrowseLsp.Status
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
    val status = new BrowseLsp.Status
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
}
