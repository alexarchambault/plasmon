package plasmon

import java.util.concurrent.ScheduledExecutorService

import scala.concurrent.{ExecutionContext, ExecutionContextExecutorService}

trait ServerThreadPools {
  def configLcEc: ExecutionContext
  def remoteLsEc: ExecutionContext
  def definitionProviderEc: ExecutionContext
  def referenceProviderEc: ExecutionContext
  def worksheetProviderEc: ExecutionContext
  def compilationEc: ExecutionContext
  def buildTargetClassesEc: ExecutionContext
  def indexingEc: ExecutionContext
  def documentChangeEc: ExecutionContext
  def batchedFunctionsEc: ExecutionContext
  def codeLensEc: ExecutionContext
  def onTypeFormattingEc: ExecutionContext
  def dummyEc: ExecutionContext
  def fileWatcherEc: ExecutionContext
  def workDoneProgressEc: ExecutionContext

  def requestsEces: ExecutionContextExecutorService

  def compilerEces: ExecutionContextExecutorService
  def cancellationEces: ExecutionContextExecutorService

  def pcThreadStopScheduler: ScheduledExecutorService
  def bspHealthCheckScheduler: ScheduledExecutorService
  def statusActorScheduler: ScheduledExecutorService

  def indexerActorContext: castor.Context
  def bspServersActorContext: castor.Context
  def statusActorContext: castor.Context
}

object ServerThreadPools {
  def noobNaiveDontUsePools(
    eces: ExecutionContextExecutorService,
    ses: ScheduledExecutorService
  ): ServerThreadPools =
    new ServerThreadPools {
      private val actorContext = new castor.Context.Test(
        eces,
        ex => scribe.error("Unhandled error in actor", ex)
      )

      def configLcEc: ExecutionContext                      = eces
      def remoteLsEc: ExecutionContext                      = eces
      def definitionProviderEc: ExecutionContext            = eces
      def referenceProviderEc: ExecutionContext             = eces
      def indexingEc: ExecutionContext                      = eces
      def documentChangeEc: ExecutionContext                = eces
      def worksheetProviderEc: ExecutionContext             = eces
      def compilationEc: ExecutionContext                   = eces
      def buildTargetClassesEc: ExecutionContext            = eces
      def batchedFunctionsEc: ExecutionContext              = eces
      def codeLensEc: ExecutionContext                      = eces
      def onTypeFormattingEc: ExecutionContext              = eces
      def dummyEc: ExecutionContext                         = eces
      def fileWatcherEc: ExecutionContext                   = eces
      def workDoneProgressEc: ExecutionContext              = eces
      def requestsEces: ExecutionContextExecutorService     = eces
      def compilerEces: ExecutionContextExecutorService     = eces
      def cancellationEces: ExecutionContextExecutorService = eces
      def pcThreadStopScheduler: ScheduledExecutorService   = ses
      def bspHealthCheckScheduler: ScheduledExecutorService = ses
      def statusActorScheduler: ScheduledExecutorService    = ses
      def indexerActorContext: castor.Context               = actorContext
      def bspServersActorContext: castor.Context            = actorContext
      def statusActorContext: castor.Context                = actorContext
    }
}
