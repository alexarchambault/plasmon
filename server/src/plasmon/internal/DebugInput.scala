package plasmon.internal

import java.io.{InputStream, OutputStream, PipedInputStream, PipedOutputStream}
import java.nio.{ByteBuffer, CharBuffer}
import java.nio.charset.StandardCharsets

object DebugInput {

  private def debug0(
    in: InputStream,
    out: OutputStream,
    onLine: String => Unit,
    threadName: String
  ): Unit = {

    val decoder    = StandardCharsets.UTF_8.newDecoder()
    val byteArray  = Array.ofDim[Byte](1024 * 1024)
    val buffer     = ByteBuffer.wrap(byteArray)
    val charArray  = Array.ofDim[Char](1024 * 1024)
    val charBuffer = CharBuffer.wrap(charArray)

    def processBuffer(): Unit = {
      val formerPos = buffer.position()
      buffer.position(0)
      val buffer0 = buffer.slice()
      buffer0.limit(formerPos)
      buffer.position(formerPos)
      decoder.decode(buffer0, charBuffer, false)
      if (buffer0.position() > 0) {
        val unread = buffer.position() - buffer0.position()
        assert(
          unread >= 0,
          s"buffer.position=${buffer.position()}, buffer0.position=${buffer0.position()}"
        )
        System.arraycopy(byteArray, buffer0.position(), byteArray, 0, unread)
        buffer.position(unread)
      }
      def processLines(startIdx: Int): Int = {
        val nlIdxOpt = (startIdx until charBuffer.position()).find { idx =>
          try
            charBuffer.get(idx) == '\n'
          catch {
            case e: IndexOutOfBoundsException =>
              throw new Exception(
                s"idx=$idx, startIdx=$startIdx, charBuffer.position=${charBuffer.position()}",
                e
              )
          }
        }
        nlIdxOpt match {
          case Some(nlIdx) =>
            val line = new String(charArray, startIdx, nlIdx - startIdx).stripSuffix("\r")
            onLine(line)
            processLines(nlIdx + 1)
          case None =>
            val line =
              new String(charArray, startIdx, charBuffer.position() - startIdx).stripSuffix("\r")
            onLine(line)
            charBuffer.position()
        }
      }
      val remainingIdx = processLines(0)
      try System.arraycopy(
          charArray,
          remainingIdx,
          charArray,
          0,
          charBuffer.position() - remainingIdx
        )
      catch {
        case e: ArrayIndexOutOfBoundsException =>
          throw new Exception(
            s"remainingIdx=$remainingIdx, charBuffer.position=${charBuffer.position()}",
            e
          )
      }
      charBuffer.position(charBuffer.position() - remainingIdx)
    }

    val t = new Thread(threadName) {
      setDaemon(true)
      override def run(): Unit =
        try {
          val buf  = Array.ofDim[Byte](16 * 1024)
          var read = -1
          while ({
            read = in.read(buf)
            read >= 0
          }) {
            out.write(buf, 0, read)
            buffer.put(buf, 0, read)
            processBuffer()
          }
        }
        catch {
          case t: Throwable =>
            scribe.error(s"Caught exception in $getName", t)
        }
    }

    t.start()
  }

  def debugIn(
    in: InputStream,
    onLine: String => Unit,
    threadName: String = "debug-input"
  ): InputStream = {
    val pipeOut = new PipedOutputStream
    debug0(in, pipeOut, onLine, threadName)
    new PipedInputStream(pipeOut)
  }

  def debugOut(
    out: OutputStream,
    onLine: String => Unit,
    threadName: String = "debug-output"
  ): OutputStream = {
    val pipeIn = new PipedInputStream
    debug0(pipeIn, out, onLine, threadName)
    new PipedOutputStream(pipeIn)
  }

  def debug(
    in: InputStream,
    out: OutputStream,
    onLine: (String, Boolean) => Unit,
    threadName: String = "debug"
  ): (InputStream, OutputStream) = {
    // val pipeIn = new PipedInputStream
    val pipeOut = new PipedOutputStream
    val res     = (new PipedInputStream(pipeOut), out) // new PipedOutputStream(pipeIn))
    debug0(in, pipeOut, onLine(_, false), threadName + "-input")
    // debug0(pipeIn, out, onLine(_, true), threadName + "-output")
    res
  }
}
