package plasmon.servercommand

import caseapp.core.RemainingArgs
import plasmon.Server
import plasmon.index.Indexer
import plasmon.protocol.CommandClient

final case class SymbolSearch(
  server: Server,
  client: CommandClient
) extends ServerCommandInstance[SymbolSearchOptions](client) {
  override def names = SymbolSearch.names
  def run(options: SymbolSearchOptions, args: RemainingArgs): Unit = {
    val arg = args.all match {
      case Seq()     => sys.error("No argument provided")
      case Seq(arg0) => arg0
      case _         => sys.error("Too many arguments provided")
    }
    val module  = ???
    val results = server.symbolSearchIndex.search(module, arg, None)
    for (info <- results) {
      val prefix = Option(info.getContainerName).getOrElse("")
      val name =
        if (options.color) SymbolSearch.highlight(info.getName, arg)
        else info.getName
      printLine(s"$prefix$name (${info.getKind})")
    }
  }
}

object SymbolSearch extends ServerCommand[SymbolSearchOptions] {
  override def names = List(
    List("symbol", "search"),
    List("symbol-search")
  )
  def instance(
    server: Server,
    indexer: Indexer,
    client: CommandClient,
    lspServer: plasmon.jsonrpc.JsonrpcServer,
    pool: plasmon.command.ServerCommandThreadPools
  ): ServerCommandInstance[SymbolSearchOptions] =
    SymbolSearch(server, client)

  def highlight(input: String, keyword: String): String = {

    def loop(input0: List[Char], keyword0: List[Char]): LazyList[fansi.Str] =
      input0 match {
        case Nil => LazyList.empty
        case h :: t =>
          keyword0 match {
            case Nil => LazyList(fansi.Str(new String(input0.toArray)))
            case kh :: kt if kh == h =>
              val elem = fansi.Bold.On(h.toString)
              elem #:: loop(t, kt)
            case _ =>
              val elem = fansi.Str(h.toString)
              elem #:: loop(t, keyword0)
          }
      }

    fansi.Str(loop(input.toCharArray.toList, keyword.toCharArray.toList)*).render
  }
}
