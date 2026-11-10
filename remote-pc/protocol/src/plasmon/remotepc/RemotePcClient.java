package plasmon.remotepc;

import java.util.List;
import java.util.concurrent.CompletableFuture;

import org.eclipse.lsp4j.Location;
import org.eclipse.lsp4j.jsonrpc.services.JsonNotification;
import org.eclipse.lsp4j.jsonrpc.services.JsonRequest;
import org.eclipse.lsp4j.jsonrpc.services.JsonSegment;

/**
 * What the Plasmon server answers, as seen from a presentation compiler host process.
 *
 * <p>These are the calls that presentation compilers make back to the server: the
 * {@code scala.meta.pc.SymbolSearch} and {@code scala.meta.pc.CompletionItemPriority} they're
 * given, logging, and the start and end of each compiler access (for progress reporting).
 */
@JsonSegment("host")
public interface RemotePcClient {

  @JsonNotification
  void log(Messages.LogParams params);

  @JsonNotification
  void accessStarted(Messages.AccessParams params);

  @JsonNotification
  void accessEnded(Messages.AccessParams params);

  /** Completes with null when there's no documentation */
  @JsonRequest
  CompletableFuture<Messages.SymbolDocumentationWire> documentation(Messages.SearchParams params);

  @JsonRequest
  CompletableFuture<List<Location>> definition(Messages.SearchParams params);

  @JsonRequest
  CompletableFuture<List<String>> definitionSourceToplevels(Messages.SearchParams params);

  /**
   * Runs a symbol search, whose candidates the server sends back one by one with
   * {@link RemotePcServer#searchVisit} before answering.
   *
   * @return the name of a {@code scala.meta.pc.SymbolSearch.Result} value
   */
  @JsonRequest
  CompletableFuture<String> search(Messages.SearchParams params);

  /** Like {@link #search}, for {@code SymbolSearch#searchMethods} */
  @JsonRequest
  CompletableFuture<String> searchMethods(Messages.SearchParams params);

  @JsonRequest
  CompletableFuture<Integer> workspaceMemberPriority(Messages.SearchParams params);
}
