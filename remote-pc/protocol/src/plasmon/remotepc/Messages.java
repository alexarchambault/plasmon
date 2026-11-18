package plasmon.remotepc;

import java.net.URI;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.TimeUnit;

import org.eclipse.lsp4j.Hover;
import org.eclipse.lsp4j.Location;
import org.eclipse.lsp4j.Range;
import org.eclipse.lsp4j.TextEdit;

import scala.meta.pc.AutoImportsResult;
import scala.meta.pc.CancelToken;
import scala.meta.pc.CompileResult;
import scala.meta.pc.ContentType;
import scala.meta.pc.DefinitionResult;
import scala.meta.pc.HoverSignature;
import scala.meta.pc.InlayHintsParams;
import scala.meta.pc.Node;
import scala.meta.pc.OffsetParams;
import scala.meta.pc.OutlineFiles;
import scala.meta.pc.ParentSymbols;
import scala.meta.pc.PcSymbolInformation;
import scala.meta.pc.PcSymbolKind;
import scala.meta.pc.PcSymbolProperty;
import scala.meta.pc.PresentationCompilerConfig;
import scala.meta.pc.RangeParams;
import scala.meta.pc.ReferencesResult;
import scala.meta.pc.SourcePathMode;
import scala.meta.pc.SymbolDocumentation;
import scala.meta.pc.VirtualFileParams;

/**
 * Messages of the protocol between the Plasmon server and presentation compiler host processes.
 *
 * <p>These are serialized by Gson, through lsp4j: only their fields matter on the wire. Those that
 * stand for a {@code scala.meta.pc} type implement it, so that they can be passed as is to
 * presentation compilers on one side, and handed back to Plasmon on the other.
 */
public final class Messages {

  private Messages() {}

  /**
   * Error code of the responses for a {@code scala.meta.pc.DisplayableException}, whose message is
   * meant to be shown to users as is.
   */
  public static final int DISPLAYABLE_ERROR_CODE = -32001;

  // ===========================================================================================
  // Lifecycle
  // ===========================================================================================

  public static final class CreateParams {
    public String pcId;
    public String module;
    public String buildTargetName;
    public String javaHome;
    public String workspace;
    public List<String> classPath;
    public List<String> options;
    public String reportsLoggerLevel;
    public ConfigWire config;
  }

  public static final class CreateResult {
    public String scalaVersion;
    public List<String> supportedCodeActions;
  }

  /** A presentation compiler request: {@code pcId} plus the fields that this request needs */
  public static final class PcRequest {
    public String pcId;
    public FileParamsWire file;
    public List<FileParamsWire> files;
    public FileParamsWire extractionPos;
    /** New name for rename, name to import for auto-imports */
    public String name;
    public Boolean isExtension;
    public String symbol;
    public org.eclipse.lsp4j.CompletionItem item;
    public String codeActionId;
    public List<Integer> argIndices;
    public String uri;
    public Boolean isHttpEnabled;
    public ReferencesRequestWire references;

    public PcRequest() {}

    public PcRequest(String pcId) {
      this.pcId = pcId;
    }
  }

  // ===========================================================================================
  // Files
  // ===========================================================================================

  /**
   * Stands for a {@link VirtualFileParams}, or one of its {@link OffsetParams}, {@link RangeParams}
   * and {@link InlayHintsParams} sub-types, depending on the fields that are set.
   */
  public static final class FileParamsWire {
    public String uri;
    public String text;
    public Integer offset;
    public Integer endOffset;
    public InlayHintsFlags inlayHints;
    public OutlineFilesWire outlineFiles;
    public boolean shouldReturnDiagnostics;

    public static FileParamsWire from(VirtualFileParams params) {
      FileParamsWire wire = new FileParamsWire();
      wire.uri = params.uri().toString();
      wire.text = params.text();
      if (params instanceof OffsetParams)
        wire.offset = ((OffsetParams) params).offset();
      if (params instanceof RangeParams)
        wire.endOffset = ((RangeParams) params).endOffset();
      if (params instanceof InlayHintsParams)
        wire.inlayHints = InlayHintsFlags.from((InlayHintsParams) params);
      wire.outlineFiles = params.outlineFiles().map(OutlineFilesWire::from).orElse(null);
      wire.shouldReturnDiagnostics = params.shouldReturnDiagnostics();
      return wire;
    }

    /** Reads these back, as an instance of the most specific type their fields allow */
    public VirtualFileParams toParams(CancelToken token) {
      if (inlayHints != null)
        return new InlayHintsParamsImpl(this, token);
      if (endOffset != null)
        return new RangeParamsImpl(this, token);
      if (offset != null)
        return new OffsetParamsImpl(this, token);
      return new VirtualFileParamsImpl(this, token);
    }

    public OffsetParams toOffsetParams(CancelToken token) {
      VirtualFileParams params = toParams(token);
      if (params instanceof OffsetParams)
        return (OffsetParams) params;
      throw new IllegalArgumentException("Expected offset parameters for " + uri);
    }

    public RangeParams toRangeParams(CancelToken token) {
      VirtualFileParams params = toParams(token);
      if (params instanceof RangeParams)
        return (RangeParams) params;
      throw new IllegalArgumentException("Expected range parameters for " + uri);
    }

    public InlayHintsParams toInlayHintsParams(CancelToken token) {
      VirtualFileParams params = toParams(token);
      if (params instanceof InlayHintsParams)
        return (InlayHintsParams) params;
      throw new IllegalArgumentException("Expected inlay hint parameters for " + uri);
    }
  }

  public static final class InlayHintsFlags {
    public boolean inferredTypes;
    public boolean typeParameters;
    public boolean implicitParameters;
    public boolean byNameParameters;
    public boolean implicitConversions;
    public boolean namedParameters;
    public boolean hintsXRayMode;
    public boolean hintsInPatternMatch;
    public boolean closingLabels;

    static InlayHintsFlags from(InlayHintsParams params) {
      InlayHintsFlags flags = new InlayHintsFlags();
      flags.inferredTypes = params.inferredTypes();
      flags.typeParameters = params.typeParameters();
      flags.implicitParameters = params.implicitParameters();
      flags.byNameParameters = params.byNameParameters();
      flags.implicitConversions = params.implicitConversions();
      flags.namedParameters = params.namedParameters();
      flags.hintsXRayMode = params.hintsXRayMode();
      flags.hintsInPatternMatch = params.hintsInPatternMatch();
      flags.closingLabels = params.closingLabels();
      return flags;
    }
  }

  public static final class OutlineFilesWire {
    public boolean isFirstCompileSubstitute;
    public List<FileParamsWire> files;

    static OutlineFilesWire from(OutlineFiles outlineFiles) {
      OutlineFilesWire wire = new OutlineFilesWire();
      wire.isFirstCompileSubstitute = outlineFiles.isFirstCompileSubstitute();
      wire.files = new ArrayList<>();
      for (VirtualFileParams file : outlineFiles.files())
        wire.files.add(FileParamsWire.from(file));
      return wire;
    }

    OutlineFiles toOutlineFiles(CancelToken token) {
      List<VirtualFileParams> files0 = new ArrayList<>();
      if (files != null)
        for (FileParamsWire file : files)
          files0.add(file.toParams(token));
      boolean isFirstCompileSubstitute0 = isFirstCompileSubstitute;
      return new OutlineFiles() {
        @Override
        public boolean isFirstCompileSubstitute() {
          return isFirstCompileSubstitute0;
        }

        @Override
        public List<VirtualFileParams> files() {
          return files0;
        }
      };
    }
  }

  private static class VirtualFileParamsImpl implements VirtualFileParams {
    final FileParamsWire wire;
    final CancelToken token;
    final Optional<OutlineFiles> outlineFiles;

    VirtualFileParamsImpl(FileParamsWire wire, CancelToken token) {
      this.wire = wire;
      this.token = token;
      this.outlineFiles = wire.outlineFiles == null
        ? Optional.empty()
        : Optional.of(wire.outlineFiles.toOutlineFiles(token));
    }

    @Override
    public URI uri() {
      return URI.create(wire.uri);
    }

    @Override
    public String text() {
      return wire.text;
    }

    @Override
    public CancelToken token() {
      return token;
    }

    @Override
    public Optional<OutlineFiles> outlineFiles() {
      return outlineFiles;
    }

    @Override
    public boolean shouldReturnDiagnostics() {
      return wire.shouldReturnDiagnostics;
    }
  }

  private static class OffsetParamsImpl extends VirtualFileParamsImpl implements OffsetParams {
    OffsetParamsImpl(FileParamsWire wire, CancelToken token) {
      super(wire, token);
    }

    @Override
    public int offset() {
      return wire.offset == null ? 0 : wire.offset;
    }
  }

  private static class RangeParamsImpl extends OffsetParamsImpl implements RangeParams {
    RangeParamsImpl(FileParamsWire wire, CancelToken token) {
      super(wire, token);
    }

    @Override
    public int endOffset() {
      return wire.endOffset == null ? offset() : wire.endOffset;
    }
  }

  private static final class InlayHintsParamsImpl extends RangeParamsImpl
      implements InlayHintsParams {
    InlayHintsParamsImpl(FileParamsWire wire, CancelToken token) {
      super(wire, token);
    }

    @Override
    public boolean inferredTypes() {
      return wire.inlayHints.inferredTypes;
    }

    @Override
    public boolean typeParameters() {
      return wire.inlayHints.typeParameters;
    }

    @Override
    public boolean implicitParameters() {
      return wire.inlayHints.implicitParameters;
    }

    @Override
    public boolean byNameParameters() {
      return wire.inlayHints.byNameParameters;
    }

    @Override
    public boolean implicitConversions() {
      return wire.inlayHints.implicitConversions;
    }

    @Override
    public boolean namedParameters() {
      return wire.inlayHints.namedParameters;
    }

    @Override
    public boolean hintsXRayMode() {
      return wire.inlayHints.hintsXRayMode;
    }

    @Override
    public boolean hintsInPatternMatch() {
      return wire.inlayHints.hintsInPatternMatch;
    }

    @Override
    public boolean closingLabels() {
      return wire.inlayHints.closingLabels;
    }
  }

  /** Stands for a {@code scala.meta.pc.ReferencesRequest} */
  public static final class ReferencesRequestWire {
    public FileParamsWire file;
    public boolean includeDefinition;
    public Integer offset;
    public String symbol;
    public List<String> alternativeSymbols;
  }

  // ===========================================================================================
  // Results
  // ===========================================================================================

  public static final class CompileResultWire implements CompileResult {
    public List<String> diagnostics;
    public String fullTree;

    public static CompileResultWire from(CompileResult result) {
      CompileResultWire wire = new CompileResultWire();
      wire.diagnostics = result.diagnostics();
      wire.fullTree = result.fullTree();
      return wire;
    }

    @Override
    public List<String> diagnostics() {
      return diagnostics == null ? Collections.emptyList() : diagnostics;
    }

    @Override
    public String fullTree() {
      return fullTree;
    }
  }

  public static final class NodeWire implements Node {
    public int start;
    public int end;
    public int tokenType;
    public int tokenModifier;

    public static NodeWire from(Node node) {
      NodeWire wire = new NodeWire();
      wire.start = node.start();
      wire.end = node.end();
      wire.tokenType = node.tokenType();
      wire.tokenModifier = node.tokenModifier();
      return wire;
    }

    @Override
    public int start() {
      return start;
    }

    @Override
    public int end() {
      return end;
    }

    @Override
    public int tokenType() {
      return tokenType;
    }

    @Override
    public int tokenModifier() {
      return tokenModifier;
    }
  }

  /**
   * Stands for a {@link HoverSignature}. The hover itself is computed on the host side, and its
   * range is the one it was given last (like {@code ScalaHover} does).
   */
  public static final class HoverWire implements HoverSignature {
    public Hover hover;
    public String signature;
    public Range range;
    public String contentType;

    public static HoverWire from(HoverSignature signature) {
      HoverWire wire = new HoverWire();
      wire.hover = signature.toLsp();
      wire.signature = signature.signature().orElse(null);
      wire.range = signature.getRange().orElse(null);
      wire.contentType = signature.contentType().name();
      return wire;
    }

    @Override
    public Hover toLsp() {
      Hover result = new Hover();
      result.setContents(hover.getContents());
      result.setRange(range);
      return result;
    }

    @Override
    public Optional<String> signature() {
      return Optional.ofNullable(signature);
    }

    @Override
    public Optional<Range> getRange() {
      return Optional.ofNullable(range);
    }

    @Override
    public HoverSignature withRange(Range range) {
      HoverWire wire = new HoverWire();
      wire.hover = hover;
      wire.signature = signature;
      wire.range = range;
      wire.contentType = contentType;
      return wire;
    }

    @Override
    public ContentType contentType() {
      return contentType == null ? ContentType.MARKDOWN : ContentType.valueOf(contentType);
    }
  }

  public static final class DefinitionResultWire implements DefinitionResult {
    public String symbol;
    public List<Location> locations;

    public static DefinitionResultWire from(DefinitionResult result) {
      DefinitionResultWire wire = new DefinitionResultWire();
      wire.symbol = result.symbol();
      wire.locations = result.locations();
      return wire;
    }

    @Override
    public String symbol() {
      return symbol;
    }

    @Override
    public List<Location> locations() {
      return locations == null ? Collections.emptyList() : locations;
    }
  }

  public static final class ReferencesResultWire implements ReferencesResult {
    public String symbol;
    public List<Location> locations;

    public static ReferencesResultWire from(ReferencesResult result) {
      ReferencesResultWire wire = new ReferencesResultWire();
      wire.symbol = result.symbol();
      wire.locations = result.locations();
      return wire;
    }

    @Override
    public String symbol() {
      return symbol;
    }

    @Override
    public List<Location> locations() {
      return locations == null ? Collections.emptyList() : locations;
    }
  }

  public static final class AutoImportsResultWire implements AutoImportsResult {
    public String packageName;
    public List<TextEdit> edits;
    public String symbol;

    public static AutoImportsResultWire from(AutoImportsResult result) {
      AutoImportsResultWire wire = new AutoImportsResultWire();
      wire.packageName = result.packageName();
      wire.edits = result.edits();
      wire.symbol = result.symbol().orElse(null);
      return wire;
    }

    @Override
    public String packageName() {
      return packageName;
    }

    @Override
    public List<TextEdit> edits() {
      return edits == null ? Collections.emptyList() : edits;
    }

    @Override
    public Optional<String> symbol() {
      return Optional.ofNullable(symbol);
    }
  }

  public static final class SymbolInformationWire implements PcSymbolInformation {
    public String symbol;
    public String kind;
    public List<String> parents;
    public String dealiasedSymbol;
    public String classOwner;
    public List<String> overriddenSymbols;
    public List<String> alternativeSymbols;
    public List<String> properties;
    public List<String> recursiveParents;
    public List<String> annotations;
    public List<String> memberDefsAnnotations;
    public List<String> typeParameters;

    public static SymbolInformationWire from(PcSymbolInformation info) {
      SymbolInformationWire wire = new SymbolInformationWire();
      wire.symbol = info.symbol();
      wire.kind = info.kind().name();
      wire.parents = info.parents();
      wire.dealiasedSymbol = info.dealiasedSymbol();
      wire.classOwner = info.classOwner();
      wire.overriddenSymbols = info.overriddenSymbols();
      wire.alternativeSymbols = info.alternativeSymbols();
      wire.properties = new ArrayList<>();
      for (PcSymbolProperty property : info.properties())
        wire.properties.add(property.name());
      wire.recursiveParents = info.recursiveParents();
      wire.annotations = info.annotations();
      wire.memberDefsAnnotations = info.memberDefsAnnotations();
      wire.typeParameters = info.typeParameters();
      return wire;
    }

    private static List<String> orEmpty(List<String> list) {
      return list == null ? Collections.emptyList() : list;
    }

    @Override
    public String symbol() {
      return symbol;
    }

    @Override
    public PcSymbolKind kind() {
      return PcSymbolKind.valueOf(kind);
    }

    @Override
    public List<String> parents() {
      return orEmpty(parents);
    }

    @Override
    public String dealiasedSymbol() {
      return dealiasedSymbol;
    }

    @Override
    public String classOwner() {
      return classOwner;
    }

    @Override
    public List<String> overriddenSymbols() {
      return orEmpty(overriddenSymbols);
    }

    @Override
    public List<String> alternativeSymbols() {
      return orEmpty(alternativeSymbols);
    }

    @Override
    public List<PcSymbolProperty> properties() {
      List<PcSymbolProperty> list = new ArrayList<>();
      for (String property : orEmpty(properties))
        list.add(PcSymbolProperty.valueOf(property));
      return list;
    }

    @Override
    public List<String> recursiveParents() {
      return orEmpty(recursiveParents);
    }

    @Override
    public List<String> annotations() {
      return orEmpty(annotations);
    }

    @Override
    public List<String> memberDefsAnnotations() {
      return orEmpty(memberDefsAnnotations);
    }

    @Override
    public List<String> typeParameters() {
      return orEmpty(typeParameters);
    }
  }

  // ===========================================================================================
  // Callbacks (from presentation compilers to the server)
  // ===========================================================================================

  public static final class LogParams {
    public String pcId;
    public String message;

    public LogParams() {}

    public LogParams(String pcId, String message) {
      this.pcId = pcId;
      this.message = message;
    }
  }

  /** Start or end of an access to a presentation compiler */
  public static final class AccessParams {
    public String pcId;
    public String requestId;
    public String name;
    public String uri;

    public AccessParams() {}

    public AccessParams(String pcId, String requestId, String name, String uri) {
      this.pcId = pcId;
      this.requestId = requestId;
      this.name = name;
      this.uri = uri;
    }
  }

  /** Parameters of the {@code scala.meta.pc.SymbolSearch} methods, plus the ones of the protocol */
  public static final class SearchParams {
    public String pcId;
    /** Identifies a {@link RemotePcClient#search} call, in the {@link VisitParams} it entails */
    public String searchId;
    public String module;
    public String query;
    public String symbol;
    public String uri;
    /** Name of a {@code scala.meta.pc.MemberKind} value */
    public String kind;
    public List<String> parents;
    /** Name of a {@link ContentType} value */
    public String contentType;

    public ParentSymbols parentSymbols() {
      List<String> parents0 = parents == null ? Collections.emptyList() : parents;
      return new ParentSymbols() {
        @Override
        public List<String> parents() {
          return parents0;
        }
      };
    }
  }

  /** One call to a {@code scala.meta.pc.SymbolSearchVisitor} visit method */
  public static final class VisitParams {
    public static final String CLASSFILE = "classfile";
    public static final String WORKSPACE_SYMBOL = "workspace-symbol";
    public static final String WORKSPACE_PACKAGE = "workspace-package";

    public String searchId;
    public String kind;
    public String pkg;
    public String filename;
    public String path;
    public String symbol;
    /** Value of a {@code org.eclipse.lsp4j.SymbolKind} */
    public Integer symbolKind;
    public Range range;
  }

  public static final class VisitResult {
    public int added;
    /** Whether the visitor says the search is cancelled, after this visit */
    public boolean cancelled;

    public VisitResult() {}

    public VisitResult(int added, boolean cancelled) {
      this.added = added;
      this.cancelled = cancelled;
    }
  }

  public static final class SymbolDocumentationWire implements SymbolDocumentation {
    public String symbol;
    public String displayName;
    public String docstring;
    public String defaultValue;
    public List<SymbolDocumentationWire> typeParameters;
    public List<SymbolDocumentationWire> parameters;

    public static SymbolDocumentationWire from(SymbolDocumentation doc) {
      SymbolDocumentationWire wire = new SymbolDocumentationWire();
      wire.symbol = doc.symbol();
      wire.displayName = doc.displayName();
      wire.docstring = doc.docstring();
      wire.defaultValue = doc.defaultValue();
      wire.typeParameters = fromList(doc.typeParameters());
      wire.parameters = fromList(doc.parameters());
      return wire;
    }

    private static List<SymbolDocumentationWire> fromList(List<SymbolDocumentation> docs) {
      List<SymbolDocumentationWire> list = new ArrayList<>();
      if (docs != null)
        for (SymbolDocumentation doc : docs)
          list.add(from(doc));
      return list;
    }

    private static List<SymbolDocumentation> toList(List<SymbolDocumentationWire> docs) {
      return docs == null ? Collections.emptyList() : new ArrayList<>(docs);
    }

    @Override
    public String symbol() {
      return symbol;
    }

    @Override
    public String displayName() {
      return displayName;
    }

    @Override
    public String docstring() {
      return docstring;
    }

    @Override
    public String defaultValue() {
      return defaultValue;
    }

    @Override
    public List<SymbolDocumentation> typeParameters() {
      return toList(typeParameters);
    }

    @Override
    public List<SymbolDocumentation> parameters() {
      return toList(parameters);
    }
  }

  // ===========================================================================================
  // Configuration
  // ===========================================================================================

  public static final class ConfigWire implements PresentationCompilerConfig {
    public String parameterHintsCommand;
    public String completionCommand;
    public Map<String, String> symbolPrefixes;
    public boolean isDefaultSymbolPrefixes;
    public String overrideDefFormat;
    public boolean isCompletionItemDetailEnabled;
    public boolean isStripMarginOnTypeFormattingEnabled;
    public boolean isCompletionItemDocumentationEnabled;
    public boolean isHoverDocumentationEnabled;
    public boolean snippetAutoIndent;
    public boolean isSignatureHelpDocumentationEnabled;
    public boolean isCompletionSnippetsEnabled;
    public boolean isDetailIncludedInLabel;
    public long timeoutDelay;
    public String timeoutUnit;
    public List<String> semanticdbCompilerOptions;
    public String hoverContentType;
    public boolean emitDiagnostics;
    public String workspaceRoot;
    public String sourcePathMode;
    public boolean shouldRunRefchecks;
    public String scalaImportsPlacement;

    public static ConfigWire from(PresentationCompilerConfig config) {
      ConfigWire wire = new ConfigWire();
      wire.parameterHintsCommand = config.parameterHintsCommand().orElse(null);
      wire.completionCommand = config.completionCommand().orElse(null);
      wire.symbolPrefixes = config.symbolPrefixes();
      wire.isDefaultSymbolPrefixes = config.isDefaultSymbolPrefixes();
      wire.overrideDefFormat = config.overrideDefFormat().name();
      wire.isCompletionItemDetailEnabled = config.isCompletionItemDetailEnabled();
      wire.isStripMarginOnTypeFormattingEnabled = config.isStripMarginOnTypeFormattingEnabled();
      wire.isCompletionItemDocumentationEnabled = config.isCompletionItemDocumentationEnabled();
      wire.isHoverDocumentationEnabled = config.isHoverDocumentationEnabled();
      wire.snippetAutoIndent = config.snippetAutoIndent();
      wire.isSignatureHelpDocumentationEnabled = config.isSignatureHelpDocumentationEnabled();
      wire.isCompletionSnippetsEnabled = config.isCompletionSnippetsEnabled();
      wire.isDetailIncludedInLabel = config.isDetailIncludedInLabel();
      wire.timeoutDelay = config.timeoutDelay();
      wire.timeoutUnit = config.timeoutUnit().name();
      wire.semanticdbCompilerOptions = config.semanticdbCompilerOptions();
      wire.hoverContentType = config.hoverContentType().name();
      wire.emitDiagnostics = config.emitDiagnostics();
      wire.workspaceRoot = config.workspaceRoot().toString();
      wire.sourcePathMode = config.sourcePathMode().name();
      wire.shouldRunRefchecks = config.shouldRunRefchecks();
      wire.scalaImportsPlacement = config.scalaImportsPlacement().name();
      return wire;
    }

    @Override
    public Optional<String> parameterHintsCommand() {
      return Optional.ofNullable(parameterHintsCommand);
    }

    @Override
    public Optional<String> completionCommand() {
      return Optional.ofNullable(completionCommand);
    }

    @Override
    public Map<String, String> symbolPrefixes() {
      return symbolPrefixes == null ? Collections.emptyMap() : symbolPrefixes;
    }

    @Override
    public boolean isDefaultSymbolPrefixes() {
      return isDefaultSymbolPrefixes;
    }

    @Override
    public OverrideDefFormat overrideDefFormat() {
      return OverrideDefFormat.valueOf(overrideDefFormat);
    }

    @Override
    public boolean isCompletionItemDetailEnabled() {
      return isCompletionItemDetailEnabled;
    }

    @Override
    public boolean isStripMarginOnTypeFormattingEnabled() {
      return isStripMarginOnTypeFormattingEnabled;
    }

    @Override
    public boolean isCompletionItemDocumentationEnabled() {
      return isCompletionItemDocumentationEnabled;
    }

    @Override
    public boolean isHoverDocumentationEnabled() {
      return isHoverDocumentationEnabled;
    }

    @Override
    public boolean snippetAutoIndent() {
      return snippetAutoIndent;
    }

    @Override
    public boolean isSignatureHelpDocumentationEnabled() {
      return isSignatureHelpDocumentationEnabled;
    }

    @Override
    public boolean isCompletionSnippetsEnabled() {
      return isCompletionSnippetsEnabled;
    }

    @Override
    public boolean isDetailIncludedInLabel() {
      return isDetailIncludedInLabel;
    }

    @Override
    public long timeoutDelay() {
      return timeoutDelay;
    }

    @Override
    public TimeUnit timeoutUnit() {
      return TimeUnit.valueOf(timeoutUnit);
    }

    @Override
    public List<String> semanticdbCompilerOptions() {
      return semanticdbCompilerOptions == null
        ? Collections.emptyList()
        : semanticdbCompilerOptions;
    }

    @Override
    public ContentType hoverContentType() {
      return ContentType.valueOf(hoverContentType);
    }

    @Override
    public boolean emitDiagnostics() {
      return emitDiagnostics;
    }

    @Override
    public Path workspaceRoot() {
      return Paths.get(workspaceRoot);
    }

    @Override
    public SourcePathMode sourcePathMode() {
      return SourcePathMode.valueOf(sourcePathMode);
    }

    @Override
    public boolean shouldRunRefchecks() {
      return shouldRunRefchecks;
    }

    @Override
    public ScalaImportsPlacement scalaImportsPlacement() {
      return ScalaImportsPlacement.valueOf(scalaImportsPlacement);
    }
  }
}
