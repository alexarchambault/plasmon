// Originally based on https://github.com/scalameta/metals/blob/2a6f8a437a1ce7c44140673edfe34bb74dfd33be/metals/src/main/scala/scala/meta/internal/metals/Diagnostics.scala#L352 or an earlier version of that file

package plasmon.bsp

import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.atomic.AtomicReference
import java.util.{ArrayList, Collections, Queue => JQueue, LinkedList}

import scala.collection.concurrent.TrieMap
import scala.collection.mutable
import scala.util.Try

import scala.meta.inputs.Input
import scala.meta.internal.metals.PositionSyntax._

import ch.epfl.scala.{bsp4j => b}
import com.google.gson.{Gson, JsonElement, JsonObject}
import org.eclipse.lsp4j.services.LanguageClient
import org.eclipse.{lsp4j => l}

import plasmon.PlasmonEnrichments._
import scala.jdk.CollectionConverters._
import plasmon.ide.Buffers
import plasmon.ide.Trees
import plasmon.ide.TokenEditDistance
import plasmon.ide.Directories
import scala.meta.internal.mtags.GlobalSymbolIndex
import plasmon.index.BspData
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import plasmon.render.JsonCodecs.given

/** (Original Metals doc) Converts diagnostics from the build server and Scalameta parser into LSP diagnostics.
  *
  * BSP diagnostics have different semantics from LSP diagnostics with regards to how they are
  * published. BSP diagnostics can be accumulated when `reset=false`, meaning the client (Metals) is
  * responsible for aggregating multiple `build/publishDiagnostics` notifications into a single
  * `textDocument/publishDiagnostics` notification.
  *
  * Another challenge is to consolidate syntax errors on every keystroke with type errors on batch
  * compile because positions get stale in batch compile errors as you type new syntax errors. To
  * solve this problem we use token edit distance the same way we support goto definition in stale
  * buffers.
  */
private final class Diagnostics(
  buffers: Buffers,
  languageClient: LanguageClient,
  workspace: os.Path,
  trees: Trees,
  private var defaultSource: String
) extends AutoCloseable {
  import Diagnostics.*

  val diagnostics           = TrieMap.empty[os.Path, JQueue[(GlobalSymbolIndex.Module, l.Diagnostic)]]
  private val syntaxError   = TrieMap.empty[os.Path, Seq[(GlobalSymbolIndex.Module, l.Diagnostic)]]
  val pcDiagnostics = TrieMap.empty[os.Path, Seq[(GlobalSymbolIndex.Module, l.Diagnostic)]]
  private val snapshots     = TrieMap.empty[os.Path, Input.VirtualFile]
  private val lastPublished = new AtomicReference[os.Path]
  private val diagnosticsBuffer =
    new ConcurrentLinkedQueue[(GlobalSymbolIndex.Module, os.Path)]
  private var enabledTypes: Set[Type] = Set(Type.Compilation, Type.Syntax)

  def setDefaultSource(name: String): Unit = {
    defaultSource = name
  }

  def diagnosticTypes: Set[Type] = enabledTypes
  def setTypes(types: Set[Type]): Unit = {
    val changed = enabledTypes != types
    if (changed) {
      enabledTypes = types
      for (path <- diagnostics.keySet ++ syntaxError.keySet ++ pcDiagnostics.keySet)
        publishDiagnostics(path, None)
    }
  }

  def onSyntaxError(
    module: GlobalSymbolIndex.Module,
    path: os.Path,
    diags: List[l.Diagnostic]
  ): Unit =
    if (path.startsWith(workspace / Directories.readonly) || diags.isEmpty) {
      if (syntaxError.remove(path).exists(_.exists(_._1 == module)))
        publishDiagnostics(path, Some(Type.Syntax)) // Remove old syntax error
    }
    else {
      syntaxError(path) = diags.map((module, _))
      publishDiagnostics(path, Some(Type.Syntax))
    }

  def onPcDiagnostics(
    module: GlobalSymbolIndex.Module,
    path: os.Path,
    diags: Seq[l.Diagnostic]
  ): Unit =
    if (path.startsWith(workspace / Directories.readonly) || diags.isEmpty) {
      if (pcDiagnostics.remove(path).exists(_.exists(_._1 == module)))
        publishDiagnostics(path, Some(Type.PresentationCompiler))
    }
    else {
      pcDiagnostics(path) = diags.map((module, _))
      publishDiagnostics(path, Some(Type.PresentationCompiler))
    }

  def onClose(module: GlobalSymbolIndex.Module, path: os.Path): Unit =
    if (syntaxError.remove(path).exists(_.exists(_._1 == module)) || pcDiagnostics.remove(path).exists(_.exists(_._1 == module)))
      publishDiagnostics(path, None) // Remove old syntax error

  def didDelete(path: os.Path): Unit = {
    diagnostics.remove(path)
    syntaxError.remove(path)
    pcDiagnostics.remove(path)
    languageClient.publishDiagnostics(
      new l.PublishDiagnosticsParams(
        path.toNIO.toUri.toASCIIString,
        Collections.emptyList()
      )
    )
  }

  def diagDidChange(path: os.Path): Unit =
    publishDiagnostics(path, None)

  def onBuildPublishDiagnostics(params: b.PublishDiagnosticsParams): Unit = {
    val diagnostics0 = params.getDiagnostics.asScala
      .toSeq
      .map { diag =>
        val diag0 = diag.toLsp
        if (diag0.getSource == null)
          diag0.setSource(defaultSource)
        diag0
      }
    val published =
      Try(params.getTextDocument.getUri.osPathFromUri).toOption.filter(os.isFile) match {
        case Some(path) =>
          val module = GlobalSymbolIndex.BuildTarget(params.getBuildTarget.getUri)
          onPublishDiagnostics(
            module,
            path,
            diagnostics0,
            params.getReset
          )
          true
        case None =>
          false
      }

    if (!published) {
      scribe.warn(
        s"Invalid text document uri received from build server: ${params.getTextDocument.getUri}"
      )
      for (diag <- diagnostics0)
        scribe.info(s"BSP server: ${diag.getMessage}")
    }
  }

  private def onPublishDiagnostics(
    module: GlobalSymbolIndex.Module,
    path: os.Path,
    diagnostics0: Seq[l.Diagnostic],
    isReset: Boolean
  ): Unit = {
    val isSamePathAsLastDiagnostic = path == lastPublished.get()
    lastPublished.set(path)
    val queue = diagnostics.getOrElseUpdate(
      path,
      new ConcurrentLinkedQueue[(GlobalSymbolIndex.Module, l.Diagnostic)]
    )
    if (isReset) {
      queue.clear()
      snapshots.remove(path)
    }
    if (queue.isEmpty && !diagnostics0.isEmpty)
      snapshots(path) = path.toInput
    diagnostics0.foreach(diagnostic => queue.add((module, diagnostic)))

    // NOTE(olafur): we buffer up several diagnostics for the same path before forwarding
    // them to the editor client. Without buffering, we risk publishing an exponential number
    // notifications for a file with N number of diagnostics:
    // Notification 1: [1]
    // Notification 2: [1, 2]
    // Notification 3: [1, 2, 3]
    // Notification N: [1, ..., N]
    if (isReset || !isSamePathAsLastDiagnostic) {
      publishDiagnosticsBuffer()
      publishDiagnostics(path, queue)
    }
    else
      diagnosticsBuffer.add((module, path))
  }

  private def publishDiagnostics(path: os.Path, changedTypeOpt: Option[Type]): Unit =
    if (changedTypeOpt.forall(enabledTypes.contains))
      publishDiagnostics(
        path,
        diagnostics.getOrElse(path, new LinkedList[(GlobalSymbolIndex.Module, l.Diagnostic)])
      )

  private def publishDiagnostics(
    path: os.Path,
    queue: JQueue[(GlobalSymbolIndex.Module, l.Diagnostic)]
  ): Unit =
    if (os.isFile(path)) {
      val uri = path.toNIO.toUri.toASCIIString
      val all = new ArrayList[l.Diagnostic]((if (enabledTypes(Type.Compilation)) queue.size() else 0) + 1)
      if (enabledTypes(Type.Compilation))
        for {
          (module0, diagnostic) <- queue.asScala
          freshDiagnostic <- toFreshDiagnostic(module0, path, diagnostic)
        }
          all.add(freshDiagnostic)
      if (enabledTypes(Type.Syntax))
        for {
          (module0, d) <- syntaxError.getOrElse(path, Nil)
          // De-duplicate only the most common and basic syntax errors.
          isSameMessage = all.asScala.exists(diag =>
            diag.getRange == d.getRange && diag.getMessage == d.getMessage
          )
          isDuplicate =
            d.getMessage.replace("`", "").startsWith("identifier expected but") &&
              all.asScala.exists { other =>
                other.getMessage
                  .replace("`", "")
                  .startsWith("identifier expected") &&
                other.getRange.getStart == d.getRange.getStart
              }
          if !isDuplicate && !isSameMessage
        }
          all.add(d)
      if (enabledTypes(Type.PresentationCompiler))
        for ((_, d) <- pcDiagnostics.getOrElse(path, Nil))
          all.add(d)
      scribe.info(s"uri=$uri")
      scribe.info(s"all=${all.asScala.toVector}")
      languageClient.publishDiagnostics(new l.PublishDiagnosticsParams(uri, all))
    }
    else
      didDelete(path)

  private def publishDiagnosticsBuffer(): Unit =
    for ((_, path) <- clearDiagnosticsBuffer())
      publishDiagnostics(path, Some(Type.Compilation))

  // Adjust positions for type errors for changes in the open buffer.
  // Only needed when merging syntax errors with type errors.
  def toFreshDiagnostic(
    module: GlobalSymbolIndex.Module,
    path: os.Path,
    d: l.Diagnostic
  ): Option[l.Diagnostic] = {
    val current  = path.toInputFromBuffers(buffers)
    val snapshot = snapshots.getOrElse(path, current)
    TokenEditDistance(module, snapshot, current, trees) match {
      case Right(edit) =>
        val result = edit
          .toRevised(
            range = d.getRange,
            adjustWithinToken = shouldAdjustWithinToken(d)
          )
          .map { range =>
            val ld = new l.Diagnostic(
              range,
              d.getMessage,
              d.getSeverity,
              d.getSource
            )
            // Scala 3 sets the diagnostic code to -1 for NoExplanation Messages. Ideally
            // this will change and we won't need this check in the future, but for now
            // let's not forward them, since they are not valid for all clients.
            val isScala3NoExplanationDiag = d.getCode != null && d
              .getCode
              .isLeft() && d.getCode.getLeft == "-1"
            if (!isScala3NoExplanationDiag) ld.setCode(d.getCode)

            ld.setTags(d.getTags)
            ld.setRelatedInformation(d.getRelatedInformation)
            ld.setCodeDescription(d.getCodeDescription)
            adjustedDiagnosticData(d, edit).map(newData => ld.setData(newData))
            ld
          }
        if (result.isEmpty)
          d.getRange.toMeta(snapshot).foreach { pos =>
            val message = pos.formatMessage(
              s"stale ${d.getSource} ${d.getSeverity.toString.toLowerCase()}",
              d.getMessage
            )
            scribe.info(message)
          }
        result

      case Left(_) =>
        // tokenization error will be shown from scalameta tokenizer
        None
    }
  }

  private def clearDiagnosticsBuffer(): Iterable[(GlobalSymbolIndex.Module, os.Path)] = {
    val toPublish = mutable.Set.empty[(GlobalSymbolIndex.Module, os.Path)]
    var elem      = diagnosticsBuffer.poll()
    while (elem != null) {
      toPublish.add(elem)
      elem = diagnosticsBuffer.poll()
    }
    toPublish
  }

  private def adjustedDiagnosticData(
    diagnostic: l.Diagnostic,
    edit: TokenEditDistance
  ): Option[Object] =
    diagnostic match {
      case ScalaDiagnostic(Left(textEdit)) =>
        edit
          .toRevised(
            textEdit,
            shouldAdjustWithinToken(diagnostic),
            fallbackToNearest = false
          )
          .map(_.toJsonObject)
      case ScalaDiagnostic(Right(scalaDiagnostic)) =>
        edit
          .toRevised(
            scalaDiagnostic,
            shouldAdjustWithinToken(diagnostic),
            fallbackToNearest = false
          )
          .map(_.toJsonObject)
      case _ => Some(diagnostic.getData)
    }

  private def shouldAdjustWithinToken(diagnostic: l.Diagnostic): Boolean =
    diagnostic.getSource == "scala-cli"

  def clear(): Unit = {
    val paths = diagnostics.keySet ++ syntaxError.keySet ++ pcDiagnostics.keySet ++ snapshots.keySet ++ diagnosticsBuffer.asScala.map(_._2)
    for (path <- paths)
      languageClient.publishDiagnostics(
        new l.PublishDiagnosticsParams(
          path.toNIO.toUri.toASCIIString,
          Collections.emptyList()
        )
      )
    diagnostics.clear()
    syntaxError.clear()
    pcDiagnostics.clear()
    snapshots.clear()
    diagnosticsBuffer.clear()
    lastPublished.set(null)
  }

  def close(): Unit = {
    val paths = diagnostics.keysIterator ++
      syntaxError.keysIterator.filterNot(diagnostics.keySet) ++
      pcDiagnostics.keysIterator.filterNot(diagnostics.keySet).filterNot(syntaxError.keySet)
    for (path <- paths)
      didDelete(path)
  }

  def asJson: Diagnostics.AsJson =
    Diagnostics.AsJson(
      diagnostics = diagnostics.toMap.map {
        case (p, l) =>
          (
            p.toString,
            l.asScala.toSeq.map {
              case (mod, diag) =>
                (mod.asString, diag)
            }
          )
      },
      syntaxError = syntaxError.toMap.map {
        case (p, l) =>
          (
            p.toString,
            l.map {
              case (mod, diag) =>
                (mod.asString, diag)
            }
          )
      },
      pcDiagnostics = pcDiagnostics.toMap.map {
        case (p, l) =>
          (
            p.toString,
            l.map {
              case (mod, diag) =>
                (mod.asString, diag)
            }
          )
      },
      snapshots = snapshots.toMap.map {
        case (p, f) =>
          (p.toString, (f.path, f.value))
      },
      lastPublished = Option(lastPublished.get()),
      diagnosticsBuffer = diagnosticsBuffer.asScala.toSeq.map {
        case (mod, p) =>
          (mod.asString, p)
      }
    )
}

object Diagnostics {

  enum Type:
    case Syntax, Compilation, PresentationCompiler

  private object ScalaDiagnostic {
    def unapply(d: l.Diagnostic): Option[Either[l.TextEdit, b.ScalaDiagnostic]] =
      d.asScalaDiagnostic
  }

  private val gson = new Gson

  private implicit class XtensionSerializableToJson(data: Any) {
    def toJson: JsonElement =
      gson.toJsonTree(data)
    def toJsonObject: JsonObject =
      data.toJson.getAsJsonObject
  }

  final case class AsJson(
    diagnostics: Map[String, Seq[(String, l.Diagnostic)]],
    syntaxError: Map[String, Seq[(String, l.Diagnostic)]],
    pcDiagnostics: Map[String, Seq[(String, l.Diagnostic)]],
    snapshots: Map[String, (String, String)],
    lastPublished: Option[os.Path],
    diagnosticsBuffer: Seq[(String, os.Path)]
  )

  private given JsonValueCodec[AsJson] =
    JsonCodecMaker.make
}
