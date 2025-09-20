package plasmon

import plasmon.languageclient.PlasmonLanguageClient

trait HasState[S] extends HasState.Outside[S] {

  private var state0         = List.empty[(S, Option[Logger])]
  private var onStateChange0 = Seq.empty[(List[S], Option[Logger]) => Unit]

  private def stateChanged(): Unit = {
    val (states, loggerOpt) = HasState.split(state0)
    onStateChange0.foreach(_(states, loggerOpt))
  }
  private def statePush(state: S, logger: Option[Logger]): Unit = {
    state0 = (state -> logger) :: state0
    stateChanged()
  }
  private def statePop(): Unit = {
    state0 = state0.tail
    stateChanged()
  }

  def languageClient: PlasmonLanguageClient
  protected def progressId: String
  protected def progressName: String

  private def withProgress[T](progress: String)(action: => T): T =
    if (progress.isEmpty) action
    else
      languageClient.reportProgress(progressId, progressName, progress) {
        action
      }

  protected def inState[T](
    state: S,
    logger: Option[Logger] = None,
    progress: String = ""
  )(f: => T): T =
    withProgress(progress) {
      statePush(state, logger)
      try f
      finally statePop()
    }

  def onStateChange(f: (List[S], Option[Logger]) => Unit): Unit = {
    onStateChange0 = onStateChange0 :+ f
  }
  def state(): (List[S], Option[Logger]) =
    HasState.split(state0)

}

object HasState {

  trait Outside[S] {
    def onStateChange(f: (List[S], Option[Logger]) => Unit): Unit
    def state(): (List[S], Option[Logger])
  }

  trait Delegate[S] extends Outside[S] {
    protected def delegateStateTo: HasState[S]

    final def onStateChange(f: (List[S], Option[Logger]) => Unit): Unit =
      delegateStateTo.onStateChange(f)
    final def state(): (List[S], Option[Logger]) =
      delegateStateTo.state()
  }

  private def split[S](states: List[(S, Option[Logger])]): (List[S], Option[Logger]) = {
    val loggerOpt = states.collectFirst { case (_, Some(logger)) => logger }
    (states.map(_._1), loggerOpt)
  }

}
