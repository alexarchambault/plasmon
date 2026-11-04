package plasmon.watch

import plasmon.index.BspData

import java.util.concurrent.{LinkedBlockingQueue, TimeUnit}

import scala.concurrent.ExecutionContext

class FileWatcherTests extends munit.FunSuite {

  private given ExecutionContext = ExecutionContext.global

  test("events are still processed after the callback threw") {
    val workspace = os.temp.dir(prefix = "plasmon-test")
    val received  = new LinkedBlockingQueue[Seq[WatchEvent]]
    val first     = WatchEvent.CreateOrModify(workspace / "A.scala")
    val second    = WatchEvent.CreateOrModify(workspace / "B.scala")
    val watcher = new FileWatcher(
      () => workspace,
      // nothing loaded, so nothing to watch on disk: the events are enqueued by hand
      new BspData(workspace, null),
      _ => true,
      Seq(_),
      events => {
        received.add(events)
        if (events.contains(first))
          throw new Exception("processing events")
      }
    )
    try {
      watcher.start()

      watcher.enqueue(first)
      assertEquals(Option(received.poll(10L, TimeUnit.SECONDS)), Some(Seq(first)))

      watcher.enqueue(second)
      assertEquals(Option(received.poll(10L, TimeUnit.SECONDS)), Some(Seq(second)))
    }
    finally {
      watcher.close()
      os.remove.all(workspace)
    }
  }
}
