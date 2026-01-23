// Originally based on https://github.com/scalameta/metals/blob/0ad0bc184f82dbd178d01f76913ea6bdfa98db14/metals/src/main/scala/scala/meta/internal/metals/Compilations.scala

package plasmon.ide

import ch.epfl.scala.{bsp4j => b}

import scala.collection.concurrent.TrieMap
import scala.collection.immutable.ListMap
import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.{Failure, Success}

import java.time.OffsetDateTime
import java.util.concurrent.CompletableFuture

import scala.jdk.CollectionConverters._

import plasmon.index.BspData
import plasmon.render.JsonCodecs.given
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import scala.util.Try

final class Compilations(
  bspData: BspData,
  afterSuccessfulCompilation: Seq[b.BuildTargetIdentifier] => Unit,
  onStartCompilation: () => Unit,
  bestEffortEnabled: Boolean
)(implicit ec: ExecutionContext) {
  import Compilations._

  private var queues = ListMap.empty[b.BuildServer, mutable.Queue[Compilation]]
  private val running =
    new mutable.HashMap[b.BuildServer, (Compilation, CompletableFuture[b.CompileResult])]
  private val queueLock = new Object

  private def checkQueue(): Unit = {
    def toProcess(): Iterator[(b.BuildServer, mutable.Queue[Compilation])] =
      queues
        .iterator
        .filter {
          case (buildServer, queue) =>
            queue.nonEmpty && !running.contains(buildServer)
        }
    if (toProcess().nonEmpty)
      queueLock.synchronized {
        for ((buildServer, queue) <- toProcess().toVector) {
          val elem = queue.dequeue()
          onStartCompilation()
          val params = new b.CompileParams(elem.targets.asJava)
          if (bestEffortEnabled)
            params.setArguments(Seq("--best-effort").asJava)
          val f         = buildServer.buildTargetCompile(params)
          val startTime = OffsetDateTime.now()
          for (target <- elem.targets)
            updateLatestCompilations(target, startTime)
          running.put(buildServer, (elem, f))
          f.whenComplete { (compileRes, ex) =>
            val endTime         = OffsetDateTime.now()
            val updatedValueOpt = Option(compileRes).map(PastCompilation(startTime, endTime, _))
            for (target <- elem.targets)
              updateLatestCompilationsDone(target, updatedValueOpt)
            val res = if (ex == null) Success(compileRes) else Failure(ex)
            if (ex == null)
              afterSuccessfulCompilation(elem.targets)
            running -= buildServer
            elem.promise.complete(res)
            checkQueue() // FIXME Run that on a specific thread pool instead?
          }
        }
      }
  }

  val latestCompilations = TrieMap.empty[
    b.BuildTargetIdentifier,
    (Option[PastCompilation], Option[OnGoingCompilation])
  ]

  def compileTarget(target: b.BuildTargetIdentifier): Future[b.CompileResult] = {

    val buildServer = bspData.buildServerOf(target).getOrElse {
      val data = bspData.allWritableData.map { data =>
        (data.buildServerOpt, data.targetToWorkspace.keysIterator.map(_.getUri).toVector.sorted)
      }
      sys.error(s"No build server found for ${target.getUri} ($data)")
    }

    val elem = Compilation(Seq(target), Promise())

    queueLock.synchronized {
      if (!queues.contains(buildServer))
        queues = queues + (buildServer -> new mutable.Queue[Compilation])
      queues(buildServer).enqueue(elem)
    }

    checkQueue()

    elem.promise.future
  }

  def compileFile(path: os.Path): Option[Future[b.CompileResult]] =
    bspData.inverseSources(path).map { targetId =>
      compileTarget(targetId)
    }

  def compileFiles(paths: Seq[os.Path]): Future[Unit] = {

    val compilations = paths
      .flatMap { path =>
        bspData.inverseSources(path).toSeq
      }
      .distinct
      .flatMap { targetId =>
        val buildServerOpt = bspData.buildServerOf(targetId)
        if (buildServerOpt.isEmpty)
          scribe.warn(s"No build server found for build target ${targetId.getUri}")
        buildServerOpt.map((_, targetId)).toSeq
      }
      .groupBy(_._1)
      .map {
        case (buildServer, values) =>
          buildServer -> Compilation(values.map(_._2), Promise())
      }

    queueLock.synchronized {
      for ((buildServer, elem) <- compilations) {
        if (!queues.contains(buildServer))
          queues = queues + (buildServer -> new mutable.Queue[Compilation])
        queues(buildServer).enqueue(elem)
      }
    }

    checkQueue()

    Future.sequence(compilations.map(_._2.promise.future)).map(_ => ())
  }

  def cancel(): Unit = {
    queueLock.synchronized {
      queues = ListMap.empty
      for ((_, (_, f)) <- running)
        f.cancel(true)
      running.clear()
    }
    latestCompilations.clear()
  }

  private def updateLatestCompilations(
    target: b.BuildTargetIdentifier,
    startTime: OffsetDateTime
  ): Unit = {
    def update(): Boolean =
      latestCompilations.get(target) match {
        case Some(previousValue @ (pastCompilationOpt, onGoingCompilationOpt)) =>
          onGoingCompilationOpt.isDefined ||
          latestCompilations.replace(
            target,
            previousValue,
            (pastCompilationOpt, Some(OnGoingCompilation(startTime)))
          )
        case None =>
          latestCompilations
            .putIfAbsent(target, (None, Some(OnGoingCompilation(startTime))))
            .isEmpty
      }
    while (!update()) {}
  }

  private def updateLatestCompilationsDone(
    target: b.BuildTargetIdentifier,
    updatedValueOpt: Option[PastCompilation]
  ): Unit = {
    def update(): Boolean =
      latestCompilations.get(target) match {
        case Some(previousValue @ (pastCompilationOpt, onGoingCompilationOpt)) =>
          (updatedValueOpt.isEmpty && onGoingCompilationOpt.isEmpty) ||
          latestCompilations.replace(
            target,
            previousValue,
            (updatedValueOpt.orElse(pastCompilationOpt), None)
          )
        case None =>
          latestCompilations
            .putIfAbsent(target, (updatedValueOpt, None))
            .isEmpty
      }
    while (!update()) {}
  }

  def asJson: Compilations.AsJson =
    Compilations.AsJson(
      queues = queues.map {
        case (k, v) =>
          (k.toString, v.toSeq.map(_.asJson))
      },
      running = running.toMap.map {
        case (k, (v0, v1)) =>
          val v1Value =
            if (v1.isCancelled())
              Left("[cancelled]")
            else if (v1.isDone())
              Try(v1.get()).toEither.left.map(ex => s"Failed: $ex")
            else
              Left("[on-going]")
          (k.toString, (v0.asJson, v1Value))
      },
      latestCompilations = latestCompilations.toMap.map {
        case (k, v) =>
          (k.getUri, v)
      }
    )
}

object Compilations {
  final case class OnGoingCompilation(
    startTime: OffsetDateTime,
    var pctDone: Double = 0.0
  )
  final case class PastCompilation(
    startTime: OffsetDateTime,
    endTime: OffsetDateTime,
    result: b.CompileResult
  )

  private final case class Compilation(
    targets: Seq[b.BuildTargetIdentifier],
    promise: Promise[b.CompileResult]
  ) {
    def asJson: CompilationAsJson =
      CompilationAsJson(
        targets = targets,
        promise = promise.future.value
          .map {
            case Success(value) => Right(value)
            case Failure(ex)    => Left(s"Exception: $ex")
          }
          .getOrElse(Left("[on-going]"))
      )
  }

  final case class CompilationAsJson(
    targets: Seq[b.BuildTargetIdentifier],
    promise: Either[String, b.CompileResult]
  )

  final case class AsJson(
    queues: ListMap[String, Seq[CompilationAsJson]],
    running: Map[String, (CompilationAsJson, Either[String, b.CompileResult])],
    latestCompilations: Map[String, (Option[PastCompilation], Option[OnGoingCompilation])]
  )

  given JsonValueCodec[AsJson] =
    JsonCodecMaker.make
}
