package plasmon.pc

import org.eclipse.lsp4j as l
import plasmon.remotepc.{Messages, RemotePcServer}

import java.net.URI
import java.nio.file.Path
import java.util.{Base64, Collections, List as JList, Optional, UUID}
import java.util.concurrent.{
  CompletableFuture,
  ConcurrentHashMap,
  ExecutorService,
  ScheduledExecutorService,
  TimeUnit
}
import java.util.function.Consumer

import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*
import scala.meta.internal.metals.EmptyCancelToken
import scala.meta.internal.mtags.GlobalSymbolIndex
import scala.meta.internal.pc.EmptySymbolSearch
import scala.meta.pc.{
  AutoImportsResult,
  CancelToken,
  CodeActionId,
  CompileResult,
  CompletionItemPriority,
  DefinitionResult,
  HoverSignature,
  InlayHintsParams,
  Node,
  OffsetParams,
  PcSymbolInformation,
  PresentationCompiler,
  PresentationCompilerConfig,
  RangeParams,
  ReferencesRequest,
  ReferencesResult,
  SymbolSearch,
  VirtualFileParams
}
import scala.util.control.NonFatal

/** A presentation compiler running in a [[RemotePcHost]]
  *
  * Like the Scala 2 presentation compiler, its `with*` and `newInstance` methods change it in
  * place. It's created on the host when it first gets a request, and created again there if the
  * host had to be started again since then.
  */
final class RemotePresentationCompiler(
  host: RemotePcHost,
  javaHome: os.Path,
  userLoggerSupplier: () => Consumer[String],
  initialModule: GlobalSymbolIndex.Module
) extends PresentationCompiler with PcAccessHooks {

  val pcId: String = UUID.randomUUID().toString

  @volatile private var moduleString             = initialModule.asString
  @volatile private var classPath                = Seq.empty[Path]
  @volatile private var options                  = Seq.empty[String]
  @volatile private var buildTargetName          = Option.empty[String]
  @volatile private var reportsLoggerLevel       = "info"
  @volatile private var workspace                = Option.empty[Path]
  @volatile private var config                   = Option.empty[PresentationCompilerConfig]
  @volatile private[pc] var search: SymbolSearch = EmptySymbolSearch
  @volatile private[pc] var completionItemPriority: CompletionItemPriority = _ => 0

  private[pc] lazy val userLogger: Consumer[String] = userLoggerSupplier()
  private[pc] def log(message: String): Unit        = userLogger.accept(message)

  def module: GlobalSymbolIndex.Module =
    GlobalSymbolIndex.Module.fromString(moduleString)

  // ===========================================================================================
  // Access hooks
  // ===========================================================================================

  @volatile private var beforeAccess0 = Option.empty[(String, String, String) => Unit]
  @volatile private var afterAccess0  = Option.empty[(String, String, String) => Unit]
  def beforeAccess(f: (String, String, String) => Unit): Unit = {
    beforeAccess0 = Some(f)
  }
  def afterAccess(f: (String, String, String) => Unit): Unit = {
    afterAccess0 = Some(f)
  }

  /** Accesses the host told us about, that it didn't say are over yet */
  private val ongoingAccesses  = new ConcurrentHashMap[String, (String, String)]
  @volatile private var loaded = false

  private[pc] def accessStarted(requestId: String, name: String, uri: String): Unit = {
    loaded = true
    ongoingAccesses.put(requestId, (name, uri))
    beforeAccess0.foreach(_(requestId, name, uri))
  }
  private[pc] def accessEnded(requestId: String, name: String, uri: String): Unit = {
    ongoingAccesses.remove(requestId)
    afterAccess0.foreach(_(requestId, name, uri))
  }

  /** Called when the host we run on exited: nothing is going on there any more */
  private[pc] def hostExited(conn: RemotePcHost#Connection): Unit =
    if (createdOn.get().exists(_ eq conn)) {
      loaded = false
      for ((requestId, (name, uri)) <- ongoingAccesses.asScala.toVector)
        accessEnded(requestId, name, uri)
    }

  // ===========================================================================================
  // Talking to the host
  // ===========================================================================================

  /** Host connection we were last created on */
  private val createdOn =
    new java.util.concurrent.atomic.AtomicReference(Option.empty[RemotePcHost#Connection])

  private def createParams(): Messages.CreateParams = {
    val params = new Messages.CreateParams
    params.pcId = pcId
    params.module = moduleString
    params.buildTargetName = buildTargetName.orNull
    params.javaHome = javaHome.toString
    params.workspace = workspace.getOrElse(os.pwd.toNIO).toString
    params.classPath = classPath.map(_.toString).asJava
    params.options = options.asJava
    params.reportsLoggerLevel = reportsLoggerLevel
    params.config = Messages.ConfigWire.from(
      config.getOrElse(scala.meta.internal.pc.PresentationCompilerConfigImpl())
    )
    params
  }

  /** Creates us on the current host if needed, and completes once that's done */
  private def created(): (RemotePcHost#Connection, CompletableFuture[Messages.CreateResult]) = {
    val conn = host.connection()
    host.register(this)
    val future = conn.created.computeIfAbsent(
      pcId,
      _ => {
        createdOn.set(Some(conn))
        conn.call(_.create(createParams()))
      }
    )
    // Try again on the next request if that failed
    future.whenComplete { (_, ex) =>
      if (ex != null)
        conn.created.remove(pcId, future)
    }
    (conn, future)
  }

  private def createResult(): Option[Messages.CreateResult] =
    try Some(created()._2.get(1L, TimeUnit.MINUTES))
    catch {
      case NonFatal(e) =>
        scribe.warn(s"Could not create presentation compiler on the host", e)
        None
    }

  /** Sends a request to the host, cancelled along with `token` */
  private def call[T](token: CancelToken)(
    f: RemotePcServer => CompletableFuture[T]
  ): CompletableFuture[T] = {
    val (conn, createdFuture) = created()
    val result                = new CompletableFuture[T]
    createdFuture.whenComplete { (_, createEx) =>
      if (createEx != null)
        result.completeExceptionally(RemotePcHost.unwrapError(createEx))
      else {
        val future = conn.call(f)
        token.onCancel().thenAccept { isCancelled =>
          if (isCancelled) future.cancel(true)
        }
        future.whenComplete { (value, ex) =>
          if (ex == null) result.complete(value)
          else result.completeExceptionally(RemotePcHost.unwrapError(ex))
        }
      }
    }
    result
  }

  /** Sends a request to the host, if we were created there (else there's nothing to do) */
  private def callIfCreated[T](f: RemotePcServer => CompletableFuture[T]): Unit =
    for (conn <- createdOn.get() if conn.isAlive && conn.created.containsKey(pcId))
      conn.call(f)

  private def request(file: VirtualFileParams): Messages.PcRequest = {
    val req = new Messages.PcRequest(pcId)
    req.file = Messages.FileParamsWire.from(file)
    req
  }

  // ===========================================================================================
  // Language features
  // ===========================================================================================

  def compile(params: VirtualFileParams): CompletableFuture[CompileResult] =
    call(params.token())(_.compile(request(params))).thenApply(r => r: CompileResult)

  override def semanticTokens(params: VirtualFileParams): CompletableFuture[JList[Node]] =
    call(params.token())(_.semanticTokens(request(params)))
      .thenApply(_.asScala.map(n => n: Node).asJava)

  def complete(params: OffsetParams): CompletableFuture[l.CompletionList] =
    call(params.token())(_.complete(request(params)))

  def completionItemResolve(
    item: l.CompletionItem,
    symbol: String
  ): CompletableFuture[l.CompletionItem] = {
    val req = new Messages.PcRequest(pcId)
    req.item = item
    req.symbol = symbol
    call(EmptyCancelToken)(_.completionItemResolve(req))
  }

  def signatureHelp(params: OffsetParams): CompletableFuture[l.SignatureHelp] =
    call(params.token())(_.signatureHelp(request(params)))

  def hover(params: OffsetParams): CompletableFuture[Optional[HoverSignature]] =
    call(params.token())(_.hover(request(params)))
      .thenApply(h => Optional.ofNullable[HoverSignature](h))

  def prepareRename(params: OffsetParams): CompletableFuture[Optional[l.Range]] =
    call(params.token())(_.prepareRename(request(params)))
      .thenApply(r => Optional.ofNullable(r))

  def rename(params: OffsetParams, name: String): CompletableFuture[JList[l.TextEdit]] = {
    val req = request(params)
    req.name = name
    call(params.token())(_.rename(req))
  }

  def definition(params: OffsetParams): CompletableFuture[DefinitionResult] =
    call(params.token())(_.definition(request(params))).thenApply(r => r: DefinitionResult)

  def typeDefinition(params: OffsetParams): CompletableFuture[DefinitionResult] =
    call(params.token())(_.typeDefinition(request(params))).thenApply(r => r: DefinitionResult)

  def documentHighlight(params: OffsetParams): CompletableFuture[JList[l.DocumentHighlight]] =
    call(params.token())(_.documentHighlight(request(params)))

  override def references(params: ReferencesRequest)
    : CompletableFuture[JList[ReferencesResult]] = {
    val ref = new Messages.ReferencesRequestWire
    ref.file = Messages.FileParamsWire.from(params.file())
    ref.includeDefinition = params.includeDefinition()
    val offsetOrSymbol = params.offsetOrSymbol()
    if (offsetOrSymbol.isLeft) ref.offset = offsetOrSymbol.getLeft
    else ref.symbol = offsetOrSymbol.getRight
    ref.alternativeSymbols = params.alternativeSymbols()
    val req = new Messages.PcRequest(pcId)
    req.references = ref
    call(params.file().token())(_.references(req))
      .thenApply(_.asScala.map(r => r: ReferencesResult).asJava)
  }

  override def codeAction[T](
    params: OffsetParams,
    codeActionId: String,
    codeActionPayload: Optional[T]
  ): CompletableFuture[JList[l.TextEdit]] = {
    val req = request(params)
    req.codeActionId = codeActionId
    codeActionPayload.toScala match {
      case Some(indices: JList[?]) if codeActionId == CodeActionId.ConvertToNamedArguments =>
        req.argIndices = indices.asScala.collect { case i: Integer => i: Integer }.asJava
      case Some(pos: OffsetParams) =>
        req.extractionPos = Messages.FileParamsWire.from(pos)
      case _ =>
    }
    call(params.token())(_.codeAction(req))
  }

  override def supportedCodeActions(): JList[String] =
    createResult().map(_.supportedCodeActions).getOrElse(Collections.emptyList())

  def getTasty(targetUri: URI, isHttpEnabled: Boolean): CompletableFuture[String] = {
    val req = new Messages.PcRequest(pcId)
    req.uri = targetUri.toString
    req.isHttpEnabled = isHttpEnabled
    call(EmptyCancelToken)(_.getTasty(req))
  }

  def autoImports(
    name: String,
    params: OffsetParams,
    isExtension: java.lang.Boolean
  ): CompletableFuture[JList[AutoImportsResult]] = {
    val req = request(params)
    req.name = name
    req.isExtension = isExtension
    call(params.token())(_.autoImports(req))
      .thenApply(_.asScala.map(r => r: AutoImportsResult).asJava)
  }

  def implementAbstractMembers(params: OffsetParams): CompletableFuture[JList[l.TextEdit]] =
    call(params.token())(_.implementAbstractMembers(request(params)))

  def insertInferredType(params: OffsetParams): CompletableFuture[JList[l.TextEdit]] =
    call(params.token())(_.insertInferredType(request(params)))

  override def inlineValue(params: OffsetParams): CompletableFuture[JList[l.TextEdit]] =
    call(params.token())(_.inlineValue(request(params)))

  def extractMethod(
    range: RangeParams,
    extractionPos: OffsetParams
  ): CompletableFuture[JList[l.TextEdit]] = {
    val req = request(range)
    req.extractionPos = Messages.FileParamsWire.from(extractionPos)
    call(range.token())(_.extractMethod(req))
  }

  def convertToNamedArguments(
    params: OffsetParams,
    argIndices: JList[Integer]
  ): CompletableFuture[JList[l.TextEdit]] = {
    val req = request(params)
    req.argIndices = argIndices
    call(params.token())(_.convertToNamedArguments(req))
  }

  def didChange(params: VirtualFileParams): CompletableFuture[JList[l.Diagnostic]] =
    call(params.token())(_.didChange(request(params)))

  override def inlayHints(params: InlayHintsParams): CompletableFuture[JList[l.InlayHint]] =
    call(params.token())(_.inlayHints(request(params)))

  override def info(symbol: String): CompletableFuture[Optional[PcSymbolInformation]] = {
    val req = new Messages.PcRequest(pcId)
    req.symbol = symbol
    call(EmptyCancelToken)(_.info(req))
      .thenApply(i => Optional.ofNullable[PcSymbolInformation](i))
  }

  def didClose(uri: URI): Unit = {
    val req = new Messages.PcRequest(pcId)
    req.uri = uri.toString
    callIfCreated(_.didClose(req))
  }

  def semanticdbTextDocument(filename: URI, code: String): CompletableFuture[Array[Byte]] =
    semanticdbTextDocument(
      scala.meta.internal.metals.CompilerVirtualFileParams(filename, code)
    )

  override def semanticdbTextDocument(params: VirtualFileParams)
    : CompletableFuture[Array[Byte]] =
    call(params.token())(_.semanticdbTextDocument(request(params)))
      .thenApply(Base64.getDecoder.decode(_))

  def selectionRange(params: JList[OffsetParams]): CompletableFuture[JList[l.SelectionRange]] = {
    val req = new Messages.PcRequest(pcId)
    req.files = params.asScala.map(Messages.FileParamsWire.from(_)).asJava
    val token = params.asScala.headOption.map(_.token()).getOrElse(EmptyCancelToken)
    call(token)(_.selectionRange(req))
  }

  // ===========================================================================================
  // Configuration and lifecycle
  // ===========================================================================================

  def shutdown(): Unit = {
    callIfCreated(_.shutdown(new Messages.PcRequest(pcId)))
    for (conn <- createdOn.get())
      conn.created.remove(pcId)
    host.unregister(this)
    loaded = false
  }

  def restart(): Unit = {
    callIfCreated(_.restart(new Messages.PcRequest(pcId)))
    loaded = false
  }

  override def withReportsLoggerLevel(level: String): PresentationCompiler = {
    reportsLoggerLevel = level
    this
  }

  override def withBuildTargetName(buildTargetName: String): PresentationCompiler = {
    this.buildTargetName = Some(buildTargetName)
    this
  }

  def withSearch(search: SymbolSearch): PresentationCompiler = {
    this.search = search
    this
  }

  // The host has executors of its own
  def withExecutorService(executorService: ExecutorService): PresentationCompiler = this
  def withScheduledExecutorService(
    scheduledExecutorService: ScheduledExecutorService
  ): PresentationCompiler = this

  def withConfiguration(config: PresentationCompilerConfig): PresentationCompiler = {
    this.config = Some(config)
    this
  }

  def withWorkspace(workspace: Path): PresentationCompiler = {
    this.workspace = Some(workspace)
    this
  }

  override def withCompletionItemPriority(priority: CompletionItemPriority)
    : PresentationCompiler = {
    completionItemPriority = priority
    this
  }

  def newInstance(
    moduleString: String,
    classpath: JList[Path],
    options: JList[String]
  ): PresentationCompiler = {
    this.moduleString = moduleString
    this.classPath = classpath.asScala.toSeq
    this.options = options.asScala.toSeq
    this
  }

  // ===========================================================================================
  // Internal methods
  // ===========================================================================================

  def diagnosticsForDebuggingPurposes(): JList[String] =
    try
      call(EmptyCancelToken)(_.diagnosticsForDebuggingPurposes(new Messages.PcRequest(pcId)))
        .get(1L, TimeUnit.MINUTES)
    catch {
      case NonFatal(e) =>
        scribe.warn("Could not get diagnostics from the host", e)
        Collections.emptyList()
    }

  def isLoaded(): Boolean = loaded

  def scalaVersion(): String =
    createdOn.get()
      .flatMap(conn => Option(conn.created.get(pcId)))
      .filter(f => f.isDone && !f.isCompletedExceptionally)
      .map(_.join().scalaVersion)
      .getOrElse(host.scalaVersion)

  override def buildTargetId(): String = module.targetId

  override def toString: String =
    s"RemotePresentationCompiler($pcId, $moduleString)"
}
