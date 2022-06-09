package com.github.morotsman
package presentation.demo

import cats.implicits._
import cats.MonadError
import cats.effect.{Ref, Temporal}
import monocle.Lens
import monocle.macros.Lenses

import scala.concurrent.duration.DurationLong

trait SourceOfMayhem[F[_]] {
  def mightFail(): F[Unit]

  def toggleFailure(): F[Unit]

  def increaseSuccessLatency(): F[Unit]

  def decreaseSuccessLatency(): F[Unit]

  def increaseRequestTimeout(): F[Unit]

  def decreaseRequestTimeout(): F[Unit]

  def mayhemState(): F[MayhemState]
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

  def make[F[_] : Temporal : MonadError[*[_], Throwable]](state: Ref[F, MayhemState]): SourceOfMayhem[F] =
    new SourceOfMayhem[F] {
      private val isFailing: Lens[MayhemState, Boolean] = MayhemState.isFailing
      private val successLatencyInMillis = MayhemState.successLatencyInMillis
      private val requestTimeoutInMillis = MayhemState.requestTimeoutInMillis

      override def mightFail(): F[Unit] =
        state.get >>= (currentState => if (!currentState.isFailing) {
          Temporal[F].sleep(currentState.successLatencyInMillis.millis)
        } else {
          Temporal[F].sleep(currentState.requestTimeoutInMillis.millis) >> MonadError[F, Throwable].raiseError(new RuntimeException("Boooom"))
        })

      override def toggleFailure(): F[Unit] =
        state.update(isFailing.modify(!_))

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
    }

}
