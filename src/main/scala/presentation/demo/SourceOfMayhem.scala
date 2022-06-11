package com.github.morotsman
package presentation.demo

import cats.implicits._
import cats.MonadError
import cats.effect.{Ref, Temporal}
import monocle.Lens
import monocle.macros.Lenses

import scala.concurrent.duration.DurationLong

trait OutcomeListener[F[_]] {
  def outcomeUpdated(isFailing: Boolean): F[Unit]
}

trait SourceOfMayhem[F[_]] {
  def mightFail(): F[Unit]

  def registerOutcomeListener(outcomeListener: OutcomeListener[F]): F[Unit]

  def toggleFailure(): F[Unit]

  def increaseSuccessLatency(): F[Unit]

  def decreaseSuccessLatency(): F[Unit]

  def increaseRequestTimeout(): F[Unit]

  def decreaseRequestTimeout(): F[Unit]

  def mayhemState(): F[MayhemState]
}

@Lenses
final case class Listeners[F[_]] (
                                   outcomeListeners: List[OutcomeListener[F]]
                                 )
object Listeners {
  def make[F[_]](): Listeners[F] = Listeners(
    outcomeListeners = List.empty
  )
}

@Lenses
final case class MayhemState(
                              isFailing: Boolean,
                              successLatencyInMillis: Long,
                              requestTimeoutInMillis: Long
                            )

object MayhemState {
  def make(): MayhemState = MayhemState(
    isFailing = false,
    successLatencyInMillis = 30,
    requestTimeoutInMillis = 1000
  )
}

object SourceOfMayhem {

  def make[F[_] : Temporal : MonadError[*[_], Throwable]](state: Ref[F, MayhemState], listeners: Ref[F, Listeners[F]]): SourceOfMayhem[F] =
    new SourceOfMayhem[F] {
      private val isFailing: Lens[MayhemState, Boolean] = MayhemState.isFailing
      private val successLatencyInMillis = MayhemState.successLatencyInMillis
      private val requestTimeoutInMillis = MayhemState.requestTimeoutInMillis
      private val outcomeListeners = Listeners.outcomeListeners[F]

      override def mightFail(): F[Unit] =
        state.get >>= (currentState => if (!currentState.isFailing) {
          Temporal[F].sleep(currentState.successLatencyInMillis.millis)
        } else {
          Temporal[F].sleep(currentState.requestTimeoutInMillis.millis) >> MonadError[F, Throwable].raiseError(new RuntimeException("Boooom"))
        })

      override def toggleFailure(): F[Unit] = for {
        updatedState <- state.updateAndGet(isFailing.modify(!_))
        allListeners <- listeners.get
        _ <- allListeners.outcomeListeners.traverse(_.outcomeUpdated(updatedState.isFailing))
      } yield ()


      override def increaseSuccessLatency(): F[Unit] =
        state.update(successLatencyInMillis.modify(_ * 2))

      override def increaseRequestTimeout(): F[Unit] =
        state.update(requestTimeoutInMillis.modify(_ * 2))

      override def decreaseSuccessLatency(): F[Unit] =
        state.update(successLatencyInMillis.modify(_ / 2))

      override def decreaseRequestTimeout(): F[Unit] =
        state.update(requestTimeoutInMillis.modify(_ / 2))

      override def mayhemState(): F[MayhemState] =
        state.get

      override def registerOutcomeListener(outcomeListener: OutcomeListener[F]): F[Unit] =
        listeners.update(
          outcomeListeners.modify(outcomeListener :: _)
        )
    }

}
