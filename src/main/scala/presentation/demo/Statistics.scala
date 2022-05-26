package com.github.morotsman
package presentation.demo

import presentation.demo.CircuitBreakerState.CircuitBreakerState

import cats.Functor
import cats.effect.{Ref, Temporal}
import cats.implicits._
import presentation.tools.{Input, NConsole}

import scala.concurrent.duration.{DurationInt, FiniteDuration}

trait StatisticsListener[F[_]] {
  def statisticsUpdated(statisticsInfo: StatisticsInfo): F[Unit]
}

trait CircuitBreakerStateListener[F[_]] {
  def circuitBreakerStateUpdated(state: CircuitBreakerState): F[Unit]
}

trait Statistics[F[_]] {
  def currentInput(input: Input): F[Unit]

  def requestSent(): F[Unit]

  def requestCompleted(): F[Unit]

  def requestCompletedIn(millis: Long): F[Unit]

  def programCompletedIn(millis: Long): F[Unit]

  def programCalled(): F[Unit]

  def circuitBreakerStateChange(state: CircuitBreakerState): F[Unit]

  def getStatisticsInfo(): F[StatisticsInfo]

  def aggregate(): F[Unit]

  def registerStatisticsListener(statisticsListener: StatisticsListener[F]): F[Unit]

  def registerCircuitBreakerStateListener(circuitBreakerStateListener: CircuitBreakerStateListener[F]): F[Unit]
}

final case class StatisticsState[F[_]]
(
  aggregated: StatisticsInfo,
  ongoing: StatisticsInfo,
  statisticsListeners: List[StatisticsListener[F]],
  circuitBreakerStateListeners: List[CircuitBreakerStateListener[F]]
)

object StatisticsState {
  def make[F[_]](): StatisticsState[F] = StatisticsState(
    ongoing = StatisticsInfo.make(),
    aggregated = StatisticsInfo.make(),
    statisticsListeners = List.empty,
    circuitBreakerStateListeners = List.empty
  )
}

final case class StatisticsInfo
(
  pendingRequests: Int,
  sentSinceLastReport: Int,
  programCalledSinceLastReport: Int,
  circuitBreakerState: CircuitBreakerState,
  requestsCompletedIn: List[Long], // TODO maybe just sum?
  programCompletedIn: List[Long],
  currentInput: Option[Input]
)

object StatisticsInfo {
  def make(): StatisticsInfo = StatisticsInfo(
    pendingRequests = 0,
    sentSinceLastReport = 0,
    programCalledSinceLastReport = 0,
    circuitBreakerState = CircuitBreakerState.CLOSED,
    requestsCompletedIn = List.empty, // TODO mem usage?
    programCompletedIn = List.empty,
    currentInput = None
  )
}

object CircuitBreakerState extends Enumeration {
  type CircuitBreakerState = Value
  val OPEN, HALF_OPEN, CLOSED = Value
}

object Statistics {

  def make[F[_] : Temporal : Functor](state: Ref[F, StatisticsState[F]]): Statistics[F] = new Statistics[F] {
    override def requestSent(): F[Unit] =
      state.modify(s => (s.copy(
        ongoing = s.ongoing.copy(
          pendingRequests = s.ongoing.pendingRequests + 1,
          sentSinceLastReport = s.ongoing.sentSinceLastReport + 1
        )
      ), s))

    override def requestCompleted(): F[Unit] =
      state.modify(s => (s.copy(
        ongoing = s.ongoing.copy(
          pendingRequests = s.ongoing.pendingRequests - 1
        )
      ), s))

    override def programCalled(): F[Unit] =
      state.modify(s => (s.copy(
        ongoing = s.ongoing.copy(
          programCalledSinceLastReport = s.ongoing.programCalledSinceLastReport + 1
        )
      ), s))

    override def circuitBreakerStateChange(circuitBreakerState: CircuitBreakerState): F[Unit] = {
      for {
        updatedState <- state.updateAndGet(s => s.copy(
          ongoing = s.ongoing.copy(
            circuitBreakerState = circuitBreakerState
          )
        ))
        _ <- updatedState.circuitBreakerStateListeners.traverse(_.circuitBreakerStateUpdated(circuitBreakerState))
      } yield ()
    }

    override def requestCompletedIn(millis: Long): F[Unit] =
      state.modify(s => (s.copy(
        ongoing = s.ongoing.copy(
          requestsCompletedIn = millis :: s.ongoing.requestsCompletedIn
        )
      ), s))

    override def programCompletedIn(millis: Long): F[Unit] =
      state.modify(s => (s.copy(
        ongoing = s.ongoing.copy(
          programCompletedIn = millis :: s.ongoing.programCompletedIn
        )
      ), s))

    override def getStatisticsInfo(): F[StatisticsInfo] =
      state.get.map(_.aggregated)

    override def aggregate(): F[Unit] = forever(1.seconds) {
      for {
        updatedState <- state.updateAndGet(state =>
          state.copy(
            aggregated = state.ongoing,
            ongoing = state.ongoing.copy(
              sentSinceLastReport = 0,
              programCalledSinceLastReport = 0,
              requestsCompletedIn = List(),
              programCompletedIn = List()
            )
          )
        )
        _ <- updatedState.statisticsListeners.traverse(_.statisticsUpdated(updatedState.aggregated.copy(
          currentInput = updatedState.ongoing.currentInput
        )))
      } yield ()
    }

    private def forever(delay: FiniteDuration)(effect: => F[_]): F[Unit] =
      Temporal[F].sleep(delay) >> effect >> forever(delay)(effect)

    override def currentInput(input: Input): F[Unit] =
      state.modify(s => (s.copy(
        ongoing = s.ongoing.copy(
          currentInput = Option(input)
        )
      ), s))

    override def registerStatisticsListener(statisticsListener: StatisticsListener[F]): F[Unit] =
      state.modify(s => (s.copy(
        statisticsListeners = statisticsListener :: s.statisticsListeners
      ), s))

    override def registerCircuitBreakerStateListener(circuitBreakerStateListener: CircuitBreakerStateListener[F]): F[Unit] =
      state.modify(s => (s.copy(
        circuitBreakerStateListeners = circuitBreakerStateListener :: s.circuitBreakerStateListeners
      ), s))
  }
}
