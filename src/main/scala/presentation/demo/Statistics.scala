package com.github.morotsman
package presentation.demo

import presentation.demo.CircuitBreakerState.CircuitBreakerState

import cats.Functor
import cats.effect.{Ref, Temporal}
import cats.implicits._
import presentation.tools.Input

import monocle.Lens
import monocle.macros.Lenses

import scala.concurrent.duration.{DurationInt, FiniteDuration}

trait CircuitBreakerStateListener[F[_]] {
  def circuitBreakerStateUpdated(state: CircuitBreakerState): F[Unit]
}

trait Statistics[F[_]] {

  def requestSent(): F[Unit]

  def requestCompleted(): F[Unit]

  def requestCompletedIn(millis: Long): F[Unit]

  def programCompletedIn(millis: Long): F[Unit]

  def programCalled(): F[Unit]

  def circuitBreakerStateChange(state: CircuitBreakerState): F[Unit]

  def getStatisticsInfo: F[StatisticsInfo]

  def aggregate(): F[Unit]

  def registerCircuitBreakerStateListener(circuitBreakerStateListener: CircuitBreakerStateListener[F]): F[Unit]
}

@Lenses
final case class StatisticsState[F[_]]
(
  aggregated: StatisticsInfo,
  ongoing: StatisticsInfo,
  circuitBreakerStateListeners: List[CircuitBreakerStateListener[F]]
)

object StatisticsState {
  def make[F[_]](): StatisticsState[F] = StatisticsState(
    ongoing = StatisticsInfo.make(),
    aggregated = StatisticsInfo.make(),
    circuitBreakerStateListeners = List.empty
  )
}

@Lenses
final case class StatisticsInfo
(
  pendingRequests: Int,
  sentSinceLastReport: Int,
  programCalledSinceLastReport: Int,
  circuitBreakerState: CircuitBreakerState,
  requestsCompletedIn: List[Long], // TODO maybe just sum?
  programCompletedIn: List[Long]
)

object StatisticsInfo {
  def make(): StatisticsInfo = StatisticsInfo(
    pendingRequests = 0,
    sentSinceLastReport = 0,
    programCalledSinceLastReport = 0,
    circuitBreakerState = CircuitBreakerState.CLOSED,
    requestsCompletedIn = List.empty, // TODO mem usage?
    programCompletedIn = List.empty
  )
}

object CircuitBreakerState extends Enumeration {
  type CircuitBreakerState = Value
  val OPEN, HALF_OPEN, CLOSED = Value
}

object Statistics {

  def make[F[_] : Temporal : Functor](state: Ref[F, StatisticsState[F]]): Statistics[F] = new Statistics[F] {

    private val ongoing: Lens[StatisticsState[F], StatisticsInfo] = StatisticsState.ongoing[F]
    private val aggregated = StatisticsState.aggregated[F]
    private val circuitBreakerStateListeners = StatisticsState.circuitBreakerStateListeners[F]

    override def requestSent(): F[Unit] = state.update(
      ongoing.andThen(StatisticsInfo.pendingRequests).modify(_ + 1) >>>
        ongoing.andThen(StatisticsInfo.sentSinceLastReport).modify(_ + 1)
    )

    override def requestCompleted(): F[Unit] = state.update(
      ongoing.andThen(StatisticsInfo.pendingRequests).modify(_ - 1)
    )

    override def programCalled(): F[Unit] = state.update(
      ongoing.andThen(StatisticsInfo.programCalledSinceLastReport).modify(_ + 1)
    )

    override def circuitBreakerStateChange(circuitBreakerState: CircuitBreakerState): F[Unit] = {
      for {
        updatedState <- state.updateAndGet(
          ongoing.andThen(StatisticsInfo.circuitBreakerState).replace(circuitBreakerState)
        )
        _ <- updatedState.circuitBreakerStateListeners.traverse(_.circuitBreakerStateUpdated(circuitBreakerState))
      } yield ()
    }

    override def requestCompletedIn(millis: Long): F[Unit] = state.update(
      ongoing.andThen(StatisticsInfo.requestsCompletedIn).modify(millis :: _)
    )

    override def programCompletedIn(millis: Long): F[Unit] = state.update(
      ongoing.andThen(StatisticsInfo.programCompletedIn).modify(millis :: _)
    )

    override def getStatisticsInfo(): F[StatisticsInfo] =
      state.get.map(_.aggregated)

    override def aggregate(): F[Unit] = forever(1.seconds) {
      for {
        _ <- state.updateAndGet { state =>
          (aggregated.replace(state.ongoing) >>>
            ongoing.andThen(StatisticsInfo.sentSinceLastReport).replace(0) >>>
            ongoing.andThen(StatisticsInfo.programCalledSinceLastReport).replace(0) >>>
            ongoing.andThen(StatisticsInfo.requestsCompletedIn).replace(List()) >>>
            ongoing.andThen(StatisticsInfo.programCompletedIn).replace(List())) (state)
        }
      } yield ()
    }

    private def forever(delay: FiniteDuration)(effect: => F[_]): F[Unit] =
      Temporal[F].sleep(delay) >> effect >> forever(delay)(effect)

    override def registerCircuitBreakerStateListener(circuitBreakerStateListener: CircuitBreakerStateListener[F]): F[Unit] =
      state.update(
        circuitBreakerStateListeners.modify(circuitBreakerStateListener :: _)
      )

  }
}
