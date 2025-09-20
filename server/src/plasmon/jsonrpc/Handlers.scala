package plasmon.jsonrpc

final case class Handlers(
  notificationHandlers: Seq[NotificationHandler[_]],
  requestHandlers: Seq[RequestHandler[_, _]],
  commandHandlers: Seq[CommandHandler]
) {
  def isEmpty: Boolean =
    notificationHandlers.isEmpty && requestHandlers.isEmpty
  def +(other: Handlers): Handlers =
    if (other.isEmpty) this
    else if (isEmpty) other
    else
      Handlers(
        notificationHandlers ++ other.notificationHandlers,
        requestHandlers ++ other.requestHandlers,
        commandHandlers ++ other.commandHandlers
      )
}

object Handlers {
  def sum(handlers: Handlers*): Handlers =
    Handlers(
      handlers.flatMap(_.notificationHandlers),
      handlers.flatMap(_.requestHandlers),
      handlers.flatMap(_.commandHandlers)
    )
}
