package plasmon.bsp

import bloop.rifle.BspConnection
import plasmon.Logger
import java.io.ByteArrayOutputStream
import java.io.PrintStream
import java.nio.charset.StandardCharsets
import plasmon.servercommand.BspUtil

import scala.annotation.nowarn

sealed abstract class BuildServerProcess extends Product with Serializable with AutoCloseable {
  def managedProcess: Boolean

  @nowarn
  def hardClose(logger: Logger): Unit =
    close()
}

object BuildServerProcess {
  final case class Process(proc: os.SubProcess, watchThread: Thread) extends BuildServerProcess {
    def managedProcess = true
    def close(): Unit = {
      scribe.info(s"Closing $this (PID: ${proc.wrapped.pid()})")
      proc.close()
      if (proc.isAlive() && !proc.waitFor(200L)) {
        scribe.info(s"Forcibly destroying PID ${proc.wrapped.pid()}")
        proc.destroyForcibly()
      }
    }
  }
  object Process {
    def createWatchThread(proc: os.SubProcess, logger: Logger): Thread = {
      val pid = proc.wrapped.pid()
      val t = new Thread(s"watch-$pid") {
        setDaemon(true)
        override def run(): Unit = {
          val maybeExited =
            try Right(proc.waitFor())
            catch {
              case _: InterruptedException =>
                Right(false)
              case ex: Throwable =>
                Left(ex)
            }
          logger.log(
            maybeExited match {
              case Right(true)  => s"Process $pid exited"
              case Right(false) => s"Watcher of process $pid interrupted"
              case Left(ex) =>
                val baos = new ByteArrayOutputStream
                ex.printStackTrace(new PrintStream(baos, true, StandardCharsets.UTF_8))
                s"Caught exception while watching process $pid" + System.lineSeparator() +
                  new String(baos.toByteArray, StandardCharsets.UTF_8)
            }
          )
        }
      }
      t
    }
  }
  final case class BloopConnection(conn: BspConnection) extends BuildServerProcess {
    def managedProcess = false
    def close(): Unit =
      conn.stop()
    override def hardClose(logger: Logger): Unit = {
      close()
      BspUtil.bloopExit(logger)
    }
  }
}
