package plasmon.handlers

import org.eclipse.{lsp4j => l}
import plasmon.jsonrpc.{Handlers, NotificationHandler}

object SetTrace {

  def handler() =
    NotificationHandler.of[l.SetTraceParams]("$/setTrace") { (params, logger) =>
      scribe.warn(s"Ignoring setTrace ${params.getValue}")
    }

  def handlers(): Handlers =
    Handlers(Seq(handler()), Nil, Nil)
}
