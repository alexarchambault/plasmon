package plasmon.integration

import scala.concurrent.duration.{Duration, DurationInt}
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success, Try}

trait PlasmonSuite extends munit.FunSuite {
  def retryCount =
    if (System.getenv("CI") == null) 0
    else 2

  override def test(options: munit.TestOptions)(body: => Any)(implicit
    loc: munit.Location
  ): Unit = {
    val className = getClass.getName
    val (classNameInit, classNameLast) = {
      val a = className.split('.')
      (a.init, a.last)
    }
    super.test(options) {
      PlasmonSuite.lock.synchronized {
        System.err.println(
          s"${Console.BLUE}Running ${classNameInit.mkString(".")}" + "." +
            s"${Console.BOLD}$classNameLast${Console.RESET}${Console.BLUE}" + "." +
            s"${Console.BOLD}${options.name}${Console.RESET}"
        )
        def handleError(ex: Throwable, willRetry: Boolean): Unit = {
          val color = if (willRetry) Console.YELLOW else Console.RED
          val msg   = if (willRetry) "Attempt failed" else "Failed"
          System.err.println(
            s"$color$msg: ${classNameInit.mkString(".")}" + "." +
              s"${Console.BOLD}$classNameLast${Console.RESET}$color" + "." +
              s"${Console.BOLD}${options.name}${Console.RESET}"
          )
          ex.printStackTrace(System.err)
        }
        val res = {
          def attempt(): Try[Any] =
            Try(body).flatMap {
              case f: Future[_] =>
                // shouldn't be a problem to await that, given that munit doesn't run tests concurrently
                Try(Await.result(f, munitTimeout))
              case other =>
                Success(other)
            }

          def helper(remaining: Int): Any =
            attempt() match {
              case Success(any) =>
                any
              case Failure(ex) =>
                val retry = remaining > 0
                handleError(ex, retry)
                if (retry)
                  helper(remaining - 1)
                else
                  throw ex
            }

          helper(retryCount)
        }
        System.err.flush()
        res
      }
    }
  }

  override def munitTimeout: Duration = 2.minutes
}

object PlasmonSuite {
  private val lock = new Object
}
