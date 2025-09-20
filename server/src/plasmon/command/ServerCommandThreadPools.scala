package plasmon.command

import bloop.rifle.BloopThreads
import plasmon.ServerThreadPools
import plasmon.util.ThreadUtil

import java.util.concurrent.{Executors, ScheduledExecutorService}

import scala.concurrent.{ExecutionContext, ExecutionContextExecutorService}

trait ServerCommandThreadPools {
  def serverPools: ServerThreadPools
  def cancelTokensEces: ExecutionContextExecutorService
  def hoverStuffEc: ExecutionContext
  def definitionStuffEc: ExecutionContext
  def bspEces: ExecutionContextExecutorService
  def dummyEc: ExecutionContext
  def statusBarSes: ScheduledExecutorService
  def bloopThreads: BloopThreads
  def clientHealthCheckScheduler: ScheduledExecutorService
}

object ServerCommandThreadPools {
  def noobNaiveDontUsePools(
    eces: ExecutionContextExecutorService,
    ses: ScheduledExecutorService
  ): ServerCommandThreadPools =
    new ServerCommandThreadPools {
      lazy val serverPools = ServerThreadPools.noobNaiveDontUsePools(eces, ses)
      def cancelTokensEces: ExecutionContextExecutorService = eces
      def hoverStuffEc: ExecutionContext                    = eces
      def definitionStuffEc: ExecutionContext               = eces
      def bspEces: ExecutionContextExecutorService          = eces
      def dummyEc: ExecutionContext                         = eces
      def statusBarSes: ScheduledExecutorService            = ses

      def bloopThreads: BloopThreads =
        BloopThreads.create()
      def clientHealthCheckScheduler: ScheduledExecutorService = ses
    }
  def noobNaiveDontUsePools(): ServerCommandThreadPools = {
    val factory = ThreadUtil.daemonThreadFactory("plasmon-stuff")
    noobNaiveDontUsePools(
      ExecutionContext.fromExecutorService(Executors.newCachedThreadPool(factory)),
      Executors.newSingleThreadScheduledExecutor(factory)
    )
  }
}
