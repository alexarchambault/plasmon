package plasmon.util

import java.util.concurrent._
import java.util.concurrent.atomic.AtomicInteger

object ThreadUtil {

  def daemonThreadFactory(namePrefix: String): ThreadFactory = {

    val threadNumber = new AtomicInteger(1)

    new ThreadFactory {
      def newThread(r: Runnable) = {
        val threadNumber0 = threadNumber.getAndIncrement
        val t             = new Thread(r, s"$namePrefix-thread-$threadNumber0")
        t.setDaemon(true)
        t.setPriority(Thread.NORM_PRIORITY)
        t
      }
    }
  }

  def fixedThreadPool(namePrefix: String, size: Int): ExecutorService = {

    val factory = daemonThreadFactory(namePrefix)

    // 1 min keep alive
    val executor = new ThreadPoolExecutor(
      size,
      size,
      1L,
      TimeUnit.MINUTES,
      new LinkedBlockingQueue[Runnable],
      factory
    )
    executor.allowCoreThreadTimeOut(true)
    executor
  }

  def fixedScheduledThreadPool(namePrefix: String, size: Int): ScheduledExecutorService = {

    val factory = daemonThreadFactory(namePrefix)

    val executor = new ScheduledThreadPoolExecutor(size, factory)
    executor.setKeepAliveTime(1L, TimeUnit.MINUTES)
    executor.allowCoreThreadTimeOut(true)
    executor
  }

  def withFixedThreadPool[T](namePrefix: String, size: Int)(f: ExecutorService => T): T = {

    var pool: ExecutorService = null
    try {
      pool = fixedThreadPool(namePrefix, size)
      f(pool)
    }
    finally if (pool != null)
        pool.shutdown()
  }

  def cachedThreadPool(namePrefix: String): ExecutorService =
    Executors.newCachedThreadPool(daemonThreadFactory(namePrefix))

  def withCachedThreadPool[T](namePrefix: String)(f: ExecutorService => T): T = {

    var pool: ExecutorService = null
    try {
      pool = cachedThreadPool(namePrefix)
      f(pool)
    }
    finally if (pool != null)
        pool.shutdown()
  }

}
