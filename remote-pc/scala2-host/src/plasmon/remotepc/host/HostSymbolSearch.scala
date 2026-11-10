package plasmon.remotepc.host

import java.net.URI
import java.nio.file.Paths
import java.util.{Collections, List => JList, Optional, UUID}
import java.util.concurrent.{
  CompletableFuture,
  ConcurrentHashMap,
  ExecutionException,
  LinkedBlockingQueue
}
import java.util.function.Consumer

import org.eclipse.lsp4j.{Location, SymbolKind}
import plasmon.remotepc.{Messages, RemotePcClient}

import scala.meta.pc.{
  CompletionItemPriority,
  ContentType,
  MemberKind,
  ParentSymbols,
  SourcePathContext,
  SymbolDocumentation,
  SymbolSearch,
  SymbolSearchVisitor
}
import scala.util.control.NonFatal

/** Searches started by presentation compilers, that wait for the server to be done with them.
  *
  * The server sends the candidates of a search back to us one by one (`pc/searchVisit`). They're
  * handed to the visitor of the search on the thread that started it, which is a presentation
  * compiler thread that waits for the search to complete. That's where the visitor expects to be
  * called: it reads the state of the compiler.
  */
final class SearchSessions {

  private final class Session(val visitor: SymbolSearchVisitor, val ctx: SourcePathContext) {
    val queue = new LinkedBlockingQueue[Runnable]
  }

  private val sessions = new ConcurrentHashMap[String, Session]

  private val done: Runnable = () => ()

  def run(
    visitor: SymbolSearchVisitor,
    ctx: SourcePathContext
  )(
    start: String => CompletableFuture[String]
  ): SymbolSearch.Result = {
    val searchId = UUID.randomUUID().toString
    val session  = new Session(visitor, ctx)
    sessions.put(searchId, session)
    try {
      val future = start(searchId)
      future.whenComplete((_, _) => session.queue.offer(done))
      var task = session.queue.take()
      while (task ne done) {
        task.run()
        task = session.queue.take()
      }
      SymbolSearch.Result.valueOf(future.get())
    }
    finally sessions.remove(searchId)
  }

  def visit(params: Messages.VisitParams): CompletableFuture[Messages.VisitResult] = {
    val result = new CompletableFuture[Messages.VisitResult]
    Option(sessions.get(params.searchId)) match {
      case None =>
        result.completeExceptionally(
          new IllegalArgumentException(s"No on-going search with id ${params.searchId}")
        )
      case Some(session) =>
        session.queue.offer { () =>
          try {
            val visitor = session.visitor
            val added = params.kind match {
              case Messages.VisitParams.CLASSFILE =>
                // The server doesn't ask our visitor about packages, and goes over all of them
                if (visitor.shouldVisitPackage(params.pkg))
                  visitor.visitClassfile(params.pkg, params.filename, session.ctx)
                else
                  0
              case Messages.VisitParams.WORKSPACE_SYMBOL =>
                visitor.visitWorkspaceSymbol(
                  Paths.get(params.path),
                  params.symbol,
                  SymbolKind.forValue(params.symbolKind),
                  params.range
                )
              case Messages.VisitParams.WORKSPACE_PACKAGE =>
                visitor.visitWorkspacePackage(params.pkg)
              case other =>
                throw new IllegalArgumentException(s"Unrecognized visit kind: $other")
            }
            result.complete(new Messages.VisitResult(added, visitor.isCancelled))
          }
          catch {
            case t: Throwable =>
              result.completeExceptionally(t)
          }
        }
    }
    result
  }
}

/** A [[SymbolSearch]] that forwards everything to the server */
final class HostSymbolSearch(
  pcId: String,
  client: () => RemotePcClient,
  sessions: SearchSessions,
  logger: Consumer[String]
) extends SymbolSearch {

  private def params(): Messages.SearchParams = {
    val params = new Messages.SearchParams
    params.pcId = pcId
    params
  }

  private def await[T](future: CompletableFuture[T]): T =
    try future.get()
    catch {
      case e: ExecutionException if e.getCause != null =>
        throw e.getCause
    }

  private def orElse[T](what: String, default: => T)(value: => T): T =
    try value
    catch {
      case e: InterruptedException =>
        throw e
      case NonFatal(e) =>
        logger.accept(s"Error getting $what from the server: $e")
        default
    }

  def documentation(
    moduleString: String,
    symbol: String,
    parents: ParentSymbols,
    logger: Consumer[String]
  ): Optional[SymbolDocumentation] =
    documentation(moduleString, symbol, parents, ContentType.MARKDOWN, logger)

  override def documentation(
    moduleString: String,
    symbol: String,
    parents: ParentSymbols,
    contentType: ContentType,
    logger: Consumer[String]
  ): Optional[SymbolDocumentation] = {
    val params0 = params()
    params0.module = moduleString
    params0.symbol = symbol
    params0.parents = parents.parents()
    params0.contentType = contentType.name
    orElse(s"documentation of $symbol", Optional.empty[SymbolDocumentation]) {
      Optional.ofNullable(await(client().documentation(params0)))
    }
  }

  def definition(moduleString: String, symbol: String, sourceUri: URI): JList[Location] = {
    val params0 = params()
    params0.module = moduleString
    params0.symbol = symbol
    params0.uri = Option(sourceUri).map(_.toString).orNull
    orElse(s"definition of $symbol", Collections.emptyList[Location]) {
      await(client().definition(params0))
    }
  }

  def definitionSourceToplevels(
    moduleString: String,
    symbol: String,
    sourceUri: URI
  ): JList[String] = {
    val params0 = params()
    params0.module = moduleString
    params0.symbol = symbol
    params0.uri = Option(sourceUri).map(_.toString).orNull
    orElse(s"top-level symbols around $symbol", Collections.emptyList[String]) {
      await(client().definitionSourceToplevels(params0))
    }
  }

  def search(
    query: String,
    moduleString: String,
    visitor: SymbolSearchVisitor,
    ctx: SourcePathContext
  ): SymbolSearch.Result =
    search(query, moduleString, Optional.empty[MemberKind](), visitor, ctx)

  override def search(
    query: String,
    buildTargetIdentifier: String,
    kind: Optional[MemberKind],
    visitor: SymbolSearchVisitor,
    ctx: SourcePathContext
  ): SymbolSearch.Result =
    orElse(s"search results for '$query'", SymbolSearch.Result.INCOMPLETE) {
      sessions.run(visitor, ctx) { searchId =>
        val params0 = params()
        params0.searchId = searchId
        params0.query = query
        params0.module = buildTargetIdentifier
        params0.kind = kind.map[String](_.name).orElse(null)
        client().search(params0)
      }
    }

  def searchMethods(
    query: String,
    buildTargetIdentifier: String,
    visitor: SymbolSearchVisitor
  ): SymbolSearch.Result =
    orElse(s"method search results for '$query'", SymbolSearch.Result.INCOMPLETE) {
      sessions.run(visitor, null) { searchId =>
        val params0 = params()
        params0.searchId = searchId
        params0.query = query
        params0.module = buildTargetIdentifier
        client().searchMethods(params0)
      }
    }
}

/** A [[CompletionItemPriority]] that asks the server, caching its answers until [[reset]] is called
  *
  * Presentation compilers ask for the priority of a symbol each time they compare it to another one
  * when sorting completions, so we only ask the server once per symbol and completion request.
  */
final class HostCompletionItemPriority(
  pcId: String,
  client: () => RemotePcClient,
  logger: Consumer[String]
) extends CompletionItemPriority {
  @volatile private var cache = new ConcurrentHashMap[String, Integer]

  def reset(): Unit = {
    cache = new ConcurrentHashMap[String, Integer]
  }

  def workspaceMemberPriority(symbol: String): Integer =
    cache.computeIfAbsent(
      symbol,
      _ => {
        val params = new Messages.SearchParams
        params.pcId = pcId
        params.symbol = symbol
        try client().workspaceMemberPriority(params).get()
        catch {
          case e: InterruptedException =>
            throw e
          case NonFatal(e) =>
            logger.accept(s"Error getting the priority of $symbol from the server: $e")
            0
        }
      }
    )
}
