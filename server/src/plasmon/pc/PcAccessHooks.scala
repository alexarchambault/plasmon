package plasmon.pc

import scala.meta.internal.pc.CompilerAccess

/** Lets us know when a presentation compiler starts or stops doing something
  *
  * The callbacks are passed a request id, a request name, and the URI of the file the request is
  * about.
  */
trait PcAccessHooks {
  def beforeAccess(f: (String, String, String) => Unit): Unit
  def afterAccess(f: (String, String, String) => Unit): Unit
}

object PcAccessHooks {
  def apply(access: CompilerAccess[?, ?]): PcAccessHooks =
    new PcAccessHooks {
      def beforeAccess(f: (String, String, String) => Unit): Unit =
        access.beforeAccess(f)
      def afterAccess(f: (String, String, String) => Unit): Unit =
        access.afterAccess(f)
    }
}
