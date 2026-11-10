package plasmon.remotepc.host

import java.io.{PrintWriter, StringWriter}
import java.net.URI
import java.nio.file.Paths
import java.util.{Base64, List => JList, Optional}
import java.util.concurrent.{
  CancellationException,
  CompletableFuture,
  CompletionException,
  ConcurrentHashMap,
  ExecutionException,
  Executors
}
import java.util.function.{Consumer, Supplier}

import org.eclipse.lsp4j.{
  CompletionItem,
  CompletionList,
  Diagnostic,
  DocumentHighlight,
  InlayHint,
  Range,
  SelectionRange,
  SignatureHelp,
  TextEdit
}
import org.eclipse.lsp4j.jsonrpc.ResponseErrorException
import org.eclipse.lsp4j.jsonrpc.messages.{Either => JEither, ResponseError, ResponseErrorCode}
import plasmon.remotepc.{Messages, RemotePcClient, RemotePcServer}

import scala.jdk.CollectionConverters._
import scala.meta.internal.mtags.GlobalSymbolIndex
import scala.meta.internal.pc.{PcReferencesRequest, ScalaPresentationCompiler}
import scala.meta.pc.{CancelToken, DisplayableException, VirtualFileParams}

/** Answers the requests of the server, by passing them to the presentation compiler they're for
  */
final class HostServer extends RemotePcServer {

  @volatile private var client0: RemotePcClient = _
  def setClient(client: RemotePcClient): Unit = {
    client0 = client
  }
  private def client(): RemotePcClient = client0

  private final class Instance(
    val pc: ScalaPresentationCompiler,
    val priority: HostCompletionItemPriority
  )

  private val instances = new ConcurrentHashMap[String, Instance]

  // Requests are handled on the thread of lsp4j that reads incoming messages: anything that
  // could block must go elsewhere, or we'd stop reading messages
  private val pool   = Executors.newCachedThreadPool(Scala2PcHost.daemonThreadFactory("host"))
  private val pcPool = Executors.newCachedThreadPool(Scala2PcHost.daemonThreadFactory("pc"))
  private val scheduler =
    Executors.newSingleThreadScheduledExecutor(Scala2PcHost.daemonThreadFactory("pc-scheduler"))

  private val searchSessions = new SearchSessions

  def shutdownAll(): Unit =
    for (instance <- instances.values().asScala)
      instance.pc.shutdown()

  /** Turns exceptions into errors that keep their message once on the server side
    *
    * (lsp4j only sends "Internal error." along with the stack trace of other exceptions)
    */
  private def responseError(t: Throwable): Throwable =
    t match {
      case e: CompletionException if e.getCause != null => responseError(e.getCause)
      case e: ExecutionException if e.getCause != null  => responseError(e.getCause)
      case e: DisplayableException =>
        new ResponseErrorException(
          new ResponseError(Messages.DISPLAYABLE_ERROR_CODE, e.getMessage, null)
        )
      case e: CancellationException  => e
      case e: ResponseErrorException => e
      case other =>
        val stackTrace = new StringWriter
        other.printStackTrace(new PrintWriter(stackTrace))
        new ResponseErrorException(
          new ResponseError(ResponseErrorCode.InternalError, other.toString, stackTrace.toString)
        )
    }

  /** Runs `f` on our pool, and completes the future that we return with its result */
  private def async[T](f: CancelToken => CompletableFuture[T]): CompletableFuture[T] = {
    val result = new CompletableFuture[T]
    val token  = new HostCancelToken(result)
    pool.execute { () =>
      try
        f(token).whenComplete { (value, ex) =>
          if (ex == null) result.complete(value)
          else result.completeExceptionally(responseError(ex))
        }
      catch {
        case t: Throwable =>
          result.completeExceptionally(responseError(t))
      }
    }
    result
  }

  private def withInstance[T](params: Messages.PcRequest)(
    f: (Instance, CancelToken) => CompletableFuture[T]
  ): CompletableFuture[T] =
    async { token =>
      Option(instances.get(params.pcId)) match {
        case Some(instance) => f(instance, token)
        case None =>
          CompletableFuture.failedFuture(
            new IllegalArgumentException(s"No presentation compiler with id ${params.pcId}")
          )
      }
    }

  private def withPc[T](params: Messages.PcRequest)(
    f: (ScalaPresentationCompiler, CancelToken) => CompletableFuture[T]
  ): CompletableFuture[T] =
    withInstance(params)((instance, token) => f(instance.pc, token))

  private def done[T](value: T): CompletableFuture[T] =
    CompletableFuture.completedFuture(value)

  private def map[T, U](future: CompletableFuture[T])(f: T => U): CompletableFuture[U] =
    future.thenApply(f(_))

  // ===========================================================================================
  // Lifecycle
  // ===========================================================================================

  def create(params: Messages.CreateParams): CompletableFuture[Messages.CreateResult] =
    async { _ =>
      val pcId = params.pcId
      val logger = new Consumer[String] {
        def accept(message: String): Unit =
          client().log(new Messages.LogParams(pcId, message))
      }
      val loggerSupplier: Supplier[Consumer[String]] = () => logger
      val module = GlobalSymbolIndex.Module.fromString(params.module)

      val pc0 = new ScalaPresentationCompiler(Paths.get(params.javaHome), loggerSupplier, module)
      pc0.compilerAccess.beforeAccess { (requestId, name, uri) =>
        client().accessStarted(new Messages.AccessParams(pcId, requestId, name, uri))
      }
      pc0.compilerAccess.afterAccess { (requestId, name, uri) =>
        client().accessEnded(new Messages.AccessParams(pcId, requestId, name, uri))
      }

      val priority = new HostCompletionItemPriority(pcId, () => client(), logger)
      val search   = new HostSymbolSearch(pcId, () => client(), searchSessions, logger)

      val pc1 = pc0
        .withSearch(search)
        .withExecutorService(pcPool)
        .withCompletionItemPriority(priority)
        .withWorkspace(Paths.get(params.workspace))
        .withScheduledExecutorService(scheduler)
        .withReportsLoggerLevel(Option(params.reportsLoggerLevel).getOrElse("info"))
        .withConfiguration(params.config)
      val pc2 = Option(params.buildTargetName).fold(pc1)(pc1.withBuildTargetName(_))
      val pc = pc2.newInstance(
        params.module,
        params.classPath.asScala.map(Paths.get(_)).asJava,
        params.options
      )

      for (previous <- Option(instances.put(pcId, new Instance(pc, priority))))
        if (previous.pc ne pc)
          previous.pc.shutdown()

      val result = new Messages.CreateResult
      result.scalaVersion = pc.scalaVersion
      result.supportedCodeActions = pc.supportedCodeActions()
      done(result)
    }

  def shutdown(params: Messages.PcRequest): CompletableFuture[Void] =
    async { _ =>
      for (instance <- Option(instances.remove(params.pcId)))
        instance.pc.shutdown()
      done(null)
    }

  def restart(params: Messages.PcRequest): CompletableFuture[Void] =
    withPc(params) { (pc, _) =>
      pc.restart()
      done(null)
    }

  def didClose(params: Messages.PcRequest): CompletableFuture[Void] =
    withPc(params) { (pc, _) =>
      pc.didClose(URI.create(params.uri))
      done(null)
    }

  def isLoaded(params: Messages.PcRequest): CompletableFuture[java.lang.Boolean] =
    withPc(params) { (pc, _) =>
      done(pc.isLoaded())
    }

  def diagnosticsForDebuggingPurposes(params: Messages.PcRequest)
    : CompletableFuture[JList[String]] =
    withPc(params) { (pc, _) =>
      done(pc.diagnosticsForDebuggingPurposes())
    }

  def exit(): Unit =
    pool.execute(() => sys.exit(0))

  // ===========================================================================================
  // Language features
  // ===========================================================================================

  private def file(params: Messages.PcRequest, token: CancelToken): VirtualFileParams =
    params.file.toParams(token)

  def compile(params: Messages.PcRequest): CompletableFuture[Messages.CompileResultWire] =
    withPc(params) { (pc, token) =>
      map(pc.compile(file(params, token)))(Messages.CompileResultWire.from(_))
    }

  def semanticTokens(params: Messages.PcRequest): CompletableFuture[JList[Messages.NodeWire]] =
    withPc(params) { (pc, token) =>
      map(pc.semanticTokens(file(params, token)))(_.asScala.map(Messages.NodeWire.from(_)).asJava)
    }

  def complete(params: Messages.PcRequest): CompletableFuture[CompletionList] =
    withInstance(params) { (instance, token) =>
      instance.priority.reset()
      instance.pc.complete(params.file.toOffsetParams(token))
    }

  def completionItemResolve(params: Messages.PcRequest): CompletableFuture[CompletionItem] =
    withPc(params) { (pc, _) =>
      pc.completionItemResolve(params.item, params.symbol)
    }

  def signatureHelp(params: Messages.PcRequest): CompletableFuture[SignatureHelp] =
    withPc(params) { (pc, token) =>
      pc.signatureHelp(params.file.toOffsetParams(token))
    }

  def hover(params: Messages.PcRequest): CompletableFuture[Messages.HoverWire] =
    withPc(params) { (pc, token) =>
      map(pc.hover(params.file.toOffsetParams(token))) { opt =>
        opt.map[Messages.HoverWire](Messages.HoverWire.from(_)).orElse(null)
      }
    }

  def prepareRename(params: Messages.PcRequest): CompletableFuture[Range] =
    withPc(params) { (pc, token) =>
      map(pc.prepareRename(params.file.toOffsetParams(token)))(_.orElse(null))
    }

  def rename(params: Messages.PcRequest): CompletableFuture[JList[TextEdit]] =
    withPc(params) { (pc, token) =>
      pc.rename(params.file.toOffsetParams(token), params.name)
    }

  def definition(params: Messages.PcRequest): CompletableFuture[Messages.DefinitionResultWire] =
    withPc(params) { (pc, token) =>
      map(pc.definition(params.file.toOffsetParams(token)))(Messages.DefinitionResultWire.from(_))
    }

  def typeDefinition(params: Messages.PcRequest)
    : CompletableFuture[Messages.DefinitionResultWire] =
    withPc(params) { (pc, token) =>
      map(pc.typeDefinition(params.file.toOffsetParams(token))) {
        Messages.DefinitionResultWire.from(_)
      }
    }

  def documentHighlight(params: Messages.PcRequest): CompletableFuture[JList[DocumentHighlight]] =
    withPc(params) { (pc, token) =>
      pc.documentHighlight(params.file.toOffsetParams(token))
    }

  def references(params: Messages.PcRequest)
    : CompletableFuture[JList[Messages.ReferencesResultWire]] =
    withPc(params) { (pc, token) =>
      val ref = params.references
      val offsetOrSymbol: JEither[Integer, String] =
        if (ref.offset != null) JEither.forLeft(ref.offset)
        else JEither.forRight(ref.symbol)
      val request = PcReferencesRequest(
        ref.file.toParams(token),
        ref.includeDefinition,
        offsetOrSymbol,
        Option(ref.alternativeSymbols).getOrElse(java.util.Collections.emptyList[String])
      )
      map(pc.references(request)) { results =>
        results.asScala.map(Messages.ReferencesResultWire.from(_)).asJava
      }
    }

  def codeAction(params: Messages.PcRequest): CompletableFuture[JList[TextEdit]] =
    withPc(params) { (pc, token) =>
      val payload: Optional[Object] =
        if (params.argIndices != null) Optional.of(params.argIndices)
        else if (params.extractionPos != null)
          Optional.of(params.extractionPos.toOffsetParams(token))
        else Optional.empty()
      // toOffsetParams gives range parameters if the server sent some (needed to extract methods)
      pc.codeAction(params.file.toOffsetParams(token), params.codeActionId, payload)
    }

  def getTasty(params: Messages.PcRequest): CompletableFuture[String] =
    withPc(params) { (pc, _) =>
      pc.getTasty(URI.create(params.uri), params.isHttpEnabled.booleanValue())
    }

  def autoImports(params: Messages.PcRequest)
    : CompletableFuture[JList[Messages.AutoImportsResultWire]] =
    withPc(params) { (pc, token) =>
      map(pc.autoImports(params.name, params.file.toOffsetParams(token), params.isExtension)) {
        results =>
          results.asScala.map(Messages.AutoImportsResultWire.from(_)).asJava
      }
    }

  def implementAbstractMembers(params: Messages.PcRequest): CompletableFuture[JList[TextEdit]] =
    withPc(params) { (pc, token) =>
      pc.implementAbstractMembers(params.file.toOffsetParams(token))
    }

  def insertInferredType(params: Messages.PcRequest): CompletableFuture[JList[TextEdit]] =
    withPc(params) { (pc, token) =>
      pc.insertInferredType(params.file.toOffsetParams(token))
    }

  def inlineValue(params: Messages.PcRequest): CompletableFuture[JList[TextEdit]] =
    withPc(params) { (pc, token) =>
      pc.inlineValue(params.file.toOffsetParams(token))
    }

  def extractMethod(params: Messages.PcRequest): CompletableFuture[JList[TextEdit]] =
    withPc(params) { (pc, token) =>
      pc.extractMethod(
        params.file.toRangeParams(token),
        params.extractionPos.toOffsetParams(token)
      )
    }

  def convertToNamedArguments(params: Messages.PcRequest): CompletableFuture[JList[TextEdit]] =
    withPc(params) { (pc, token) =>
      pc.convertToNamedArguments(params.file.toOffsetParams(token), params.argIndices)
    }

  def didChange(params: Messages.PcRequest): CompletableFuture[JList[Diagnostic]] =
    withPc(params) { (pc, token) =>
      pc.didChange(file(params, token))
    }

  def inlayHints(params: Messages.PcRequest): CompletableFuture[JList[InlayHint]] =
    withPc(params) { (pc, token) =>
      pc.inlayHints(params.file.toInlayHintsParams(token))
    }

  def info(params: Messages.PcRequest): CompletableFuture[Messages.SymbolInformationWire] =
    withPc(params) { (pc, _) =>
      map(pc.info(params.symbol)) { opt =>
        opt.map[Messages.SymbolInformationWire](Messages.SymbolInformationWire.from(_)).orElse(null)
      }
    }

  def semanticdbTextDocument(params: Messages.PcRequest): CompletableFuture[String] =
    withPc(params) { (pc, token) =>
      map(pc.semanticdbTextDocument(file(params, token)))(Base64.getEncoder.encodeToString(_))
    }

  def selectionRange(params: Messages.PcRequest): CompletableFuture[JList[SelectionRange]] =
    withPc(params) { (pc, token) =>
      pc.selectionRange(params.files.asScala.map(_.toOffsetParams(token)).asJava)
    }

  def searchVisit(params: Messages.VisitParams): CompletableFuture[Messages.VisitResult] =
    searchSessions.visit(params)
}
