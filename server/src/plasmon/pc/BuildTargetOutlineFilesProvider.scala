// Originally based on https://github.com/scalameta/metals/blob/0ad0bc184f82dbd178d01f76913ea6bdfa98db14/metals/src/main/scala/scala/meta/internal/metals/OutlineFilesProvider.scala or earlier versions of that file

package plasmon.pc

import ch.epfl.scala.bsp4j as b
import plasmon.ide.Buffers
import plasmon.index.BspData

import java.lang.Boolean as JBoolean
import java.util.Collections
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicBoolean

import scala.jdk.CollectionConverters.*
import scala.meta.internal.metals.CompilerVirtualFileParams
import scala.meta.pc.VirtualFileParams

private class BuildTargetOutlineFilesProvider(
  bspData: BspData,
  buffers: Buffers,
  id: b.BuildTargetIdentifier,
  wasCompilationSuccessful: Boolean
) {
  private val changedDocuments =
    Collections.newSetFromMap(new ConcurrentHashMap[os.Path, JBoolean])

  private val wasAllOutlined: AtomicBoolean =
    new AtomicBoolean(false)

  private val wasSuccessfullyCompiled: AtomicBoolean =
    new AtomicBoolean(wasCompilationSuccessful)

  def wasSuccessfullyCompiledByBuildServer: Boolean =
    wasSuccessfullyCompiled.get()

  def successfulCompilation(): Unit = {
    wasSuccessfullyCompiled.set(true)
    changedDocuments.clear()
  }

  def didChange(path: os.Path): Boolean =
    changedDocuments.add(path)

  def outlineFiles(): Option[OutlineFiles] =
    if (!wasSuccessfullyCompiled.get() && !wasAllOutlined.getAndSet(true)) {
      // initial outline compilation that is a substitute for build server compilation
      val allFiles =
        bspData
          .buildTargetSources(id)
          .flatMap(os.walk(_))

      if (allFiles.size > BuildTargetOutlineFilesProvider.maxOutlineFiles)
        // too many files to outline using pc
        None
      else
        Some(
          OutlineFiles(
            allFiles.flatMap(toVirtualFileParams(_)).toList.asJava,
            isFirstCompileSubstitute = true
          )
        )
    }
    else
      changedDocuments.asScala.toList.flatMap(
        toVirtualFileParams
      ) match {
        case Nil => None
        case files =>
          Some(
            OutlineFiles(
              files.asJava,
              isFirstCompileSubstitute = false
            )
          )
      }

  private def toVirtualFileParams(path: os.Path): Option[VirtualFileParams] =
    buffers.get(path).orElse(Option.when(os.exists(path))(os.read(path))).map { text =>
      CompilerVirtualFileParams(path.toNIO.toUri, text)
    }
}

private object BuildTargetOutlineFilesProvider {
  private def maxOutlineFiles = 300
}
