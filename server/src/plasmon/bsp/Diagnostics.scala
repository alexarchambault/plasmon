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

/** Converts diagnostics from the build server and Scalameta parser into LSP diagnostics.
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
  trees: Trees
) extends AutoCloseable {
  import Diagnostics.*

  val diagnostics           = TrieMap.empty[os.Path, JQueue[l.Diagnostic]]
  private val syntaxError   = TrieMap.empty[os.Path, Seq[l.Diagnostic]]
  private val snapshots     = TrieMap.empty[os.Path, Input.VirtualFile]
  private val lastPublished = new AtomicReference[os.Path]
  private val diagnosticsBuffer =
    new ConcurrentLinkedQueue[(GlobalSymbolIndex.Module, os.Path)]

  def onSyntaxError(
    module: GlobalSymbolIndex.Module,
    path: os.Path,
    diags: List[l.Diagnostic]
  ): Unit =
    if (path.startsWith(workspace / Directories.readonly) || diags.isEmpty)
      onClose(module, path)
    else {
      syntaxError(path) = diags
      publishDiagnostics(module, path)
    }

  def onClose(module: GlobalSymbolIndex.Module, path: os.Path): Unit =
    if (syntaxError.remove(path).isDefined)
      publishDiagnostics(module, path) // Remove old syntax error

  def didDelete(path: os.Path): Unit = {
    diagnostics.remove(path)
    syntaxError.remove(path)
    languageClient.publishDiagnostics(
      new l.PublishDiagnosticsParams(
        path.toNIO.toUri.toASCIIString,
        Collections.emptyList()
      )
    )
  }

  def diagDidChange(module: GlobalSymbolIndex.Module, path: os.Path): Unit =
    publishDiagnostics(module, path)

  def onBuildPublishDiagnostics(params: b.PublishDiagnosticsParams): Unit = {
    val diagnostics = params.getDiagnostics.asScala.map(_.toLsp).toSeq
    val published =
      Try(params.getTextDocument.getUri.osPathFromUri).toOption.filter(os.isFile) match {
        case Some(path) =>
          val module = GlobalSymbolIndex.BuildTarget(params.getBuildTarget.getUri)
          onPublishDiagnostics(
            module,
            path,
            diagnostics,
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
      for (diag <- diagnostics)
        scribe.info(s"BSP server: ${diag.getMessage}")
    }
  }

  private def onPublishDiagnostics(
    module: GlobalSymbolIndex.Module,
    path: os.Path,
    diagnostics: Seq[l.Diagnostic],
    isReset: Boolean
  ): Unit = {
    val isSamePathAsLastDiagnostic = path == lastPublished.get()
    lastPublished.set(path)
    val queue = this.diagnostics.getOrElseUpdate(path, new ConcurrentLinkedQueue[l.Diagnostic])
    if (isReset) {
      queue.clear()
      snapshots.remove(path)
    }
    if (queue.isEmpty && !diagnostics.isEmpty)
      snapshots(path) = path.toInput
    diagnostics.foreach(diagnostic => queue.add(diagnostic))

    // NOTE(olafur): we buffer up several diagnostics for the same path before forwarding
    // them to the editor client. Without buffering, we risk publishing an exponential number
    // notifications for a file with N number of diagnostics:
    // Notification 1: [1]
    // Notification 2: [1, 2]
    // Notification 3: [1, 2, 3]
    // Notification N: [1, ..., N]
    if (isReset || !isSamePathAsLastDiagnostic) {
      publishDiagnosticsBuffer()
      publishDiagnostics(module, path, queue)
    }
    else
      diagnosticsBuffer.add((module, path))
  }

  private def publishDiagnostics(module: GlobalSymbolIndex.Module, path: os.Path): Unit =
    publishDiagnostics(
      module,
      path,
      diagnostics.getOrElse(path, new LinkedList[l.Diagnostic])
    )

  private def publishDiagnostics(
    module: GlobalSymbolIndex.Module,
    path: os.Path,
    queue: JQueue[l.Diagnostic]
  ): Unit =
    if (os.isFile(path)) {
      val uri = path.toNIO.toUri.toASCIIString
      val all = new ArrayList[l.Diagnostic](queue.size() + 1)
      for {
        diagnostic      <- queue.asScala
        freshDiagnostic <- toFreshDiagnostic(module, path, diagnostic)
      }
        all.add(freshDiagnostic)
      for {
        d <- syntaxError.getOrElse(path, Nil)
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
      scribe.info(s"uri=$uri")
      scribe.info(s"all=${all.asScala.toVector}")
      languageClient.publishDiagnostics(new l.PublishDiagnosticsParams(uri, all))
    }
    else
      didDelete(path)

  private def publishDiagnosticsBuffer(): Unit =
    for ((mod, path) <- clearDiagnosticsBuffer())
      publishDiagnostics(mod, path)

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

  def close(): Unit = {
    val paths = diagnostics.keysIterator ++ syntaxError.keysIterator.filterNot(diagnostics.keySet)
    for (path <- paths)
      didDelete(path)
  }
}

private object Diagnostics {
  private object ScalaDiagnostic {
    def unapply(d: l.Diagnostic): Option[Either[l.TextEdit, b.ScalaDiagnostic]] =
      d.asScalaDiagnostic
  }

  private val gson = new Gson

  implicit class XtensionSerializableToJson(data: Any) {
    def toJson: JsonElement =
      gson.toJsonTree(data)
    def toJsonObject: JsonObject =
      data.toJson.getAsJsonObject
  }
}
