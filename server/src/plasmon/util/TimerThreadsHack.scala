package plasmon.util

import scala.jdk.CollectionConverters.*

object TimerThreadsHack {
  def cleanup(): Unit =
    Thread.getAllStackTraces.keySet().asScala.foreach {
      case t if !t.isDaemon && t.getClass.getName == "java.util.TimerThread" =>
        // pprint.err.log(t.getClass.getDeclaredFields)
        val field = t.getClass.getDeclaredField("newTasksMayBeScheduled")
        field.setAccessible(true)
        field.set(t, false)
        // System.err.println(s"Interrupting $t")
        t.interrupt()
      case _ =>
    }
}
