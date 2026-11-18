package plasmon.remotepc;

import java.util.List;
import java.util.concurrent.CompletableFuture;

import org.eclipse.lsp4j.CompletionItem;
import org.eclipse.lsp4j.CompletionList;
import org.eclipse.lsp4j.Diagnostic;
import org.eclipse.lsp4j.DocumentHighlight;
import org.eclipse.lsp4j.InlayHint;
import org.eclipse.lsp4j.Range;
import org.eclipse.lsp4j.SelectionRange;
import org.eclipse.lsp4j.SignatureHelp;
import org.eclipse.lsp4j.TextEdit;
import org.eclipse.lsp4j.jsonrpc.services.JsonNotification;
import org.eclipse.lsp4j.jsonrpc.services.JsonRequest;
import org.eclipse.lsp4j.jsonrpc.services.JsonSegment;

/**
 * What a presentation compiler host process answers, as seen from the Plasmon server.
 *
 * <p>A host runs any number of presentation compilers, each identified by the {@code pcId} that
 * {@link #create} was given. Most methods mirror those of {@code scala.meta.pc.PresentationCompiler}.
 */
@JsonSegment("pc")
public interface RemotePcServer {

  @JsonRequest
  CompletableFuture<Messages.CreateResult> create(Messages.CreateParams params);

  @JsonRequest
  CompletableFuture<Void> shutdown(Messages.PcRequest params);

  @JsonRequest
  CompletableFuture<Void> restart(Messages.PcRequest params);

  @JsonRequest
  CompletableFuture<Void> didClose(Messages.PcRequest params);

  @JsonRequest
  CompletableFuture<Boolean> isLoaded(Messages.PcRequest params);

  @JsonRequest
  CompletableFuture<List<String>> diagnosticsForDebuggingPurposes(Messages.PcRequest params);

  @JsonRequest
  CompletableFuture<Messages.CompileResultWire> compile(Messages.PcRequest params);

  @JsonRequest
  CompletableFuture<List<Messages.NodeWire>> semanticTokens(Messages.PcRequest params);

  @JsonRequest
  CompletableFuture<CompletionList> complete(Messages.PcRequest params);

  @JsonRequest
  CompletableFuture<CompletionItem> completionItemResolve(Messages.PcRequest params);

  @JsonRequest
  CompletableFuture<SignatureHelp> signatureHelp(Messages.PcRequest params);

  /** Completes with null when there's nothing to show */
  @JsonRequest
  CompletableFuture<Messages.HoverWire> hover(Messages.PcRequest params);

  /** Completes with null when there's nothing to rename */
  @JsonRequest
  CompletableFuture<Range> prepareRename(Messages.PcRequest params);

  @JsonRequest
  CompletableFuture<List<TextEdit>> rename(Messages.PcRequest params);

  @JsonRequest
  CompletableFuture<Messages.DefinitionResultWire> definition(Messages.PcRequest params);

  @JsonRequest
  CompletableFuture<Messages.DefinitionResultWire> typeDefinition(Messages.PcRequest params);

  @JsonRequest
  CompletableFuture<List<DocumentHighlight>> documentHighlight(Messages.PcRequest params);

  @JsonRequest
  CompletableFuture<List<Messages.ReferencesResultWire>> references(Messages.PcRequest params);

  @JsonRequest
  CompletableFuture<List<TextEdit>> codeAction(Messages.PcRequest params);

  @JsonRequest
  CompletableFuture<String> getTasty(Messages.PcRequest params);

  @JsonRequest
  CompletableFuture<List<Messages.AutoImportsResultWire>> autoImports(Messages.PcRequest params);

  @JsonRequest
  CompletableFuture<List<TextEdit>> implementAbstractMembers(Messages.PcRequest params);

  @JsonRequest
  CompletableFuture<List<TextEdit>> insertInferredType(Messages.PcRequest params);

  @JsonRequest
  CompletableFuture<List<TextEdit>> inlineValue(Messages.PcRequest params);

  @JsonRequest
  CompletableFuture<List<TextEdit>> extractMethod(Messages.PcRequest params);

  @JsonRequest
  CompletableFuture<List<TextEdit>> convertToNamedArguments(Messages.PcRequest params);

  @JsonRequest
  CompletableFuture<List<Diagnostic>> didChange(Messages.PcRequest params);

  @JsonRequest
  CompletableFuture<List<InlayHint>> inlayHints(Messages.PcRequest params);

  /** Completes with null when the symbol isn't known */
  @JsonRequest
  CompletableFuture<Messages.SymbolInformationWire> info(Messages.PcRequest params);

  /** The SemanticDB text document, as base64 */
  @JsonRequest
  CompletableFuture<String> semanticdbTextDocument(Messages.PcRequest params);

  @JsonRequest
  CompletableFuture<List<SelectionRange>> selectionRange(Messages.PcRequest params);

  /**
   * Visits one search candidate with the visitor of an on-going {@link RemotePcClient#search} /
   * {@link RemotePcClient#searchMethods} call.
   *
   * <p>The host answers it on the thread that is blocked on that search call, so that the visitor
   * runs where the presentation compiler expects it to.
   */
  @JsonRequest
  CompletableFuture<Messages.VisitResult> searchVisit(Messages.VisitParams params);

  /** Asks the host to exit */
  @JsonNotification
  void exit();
}
