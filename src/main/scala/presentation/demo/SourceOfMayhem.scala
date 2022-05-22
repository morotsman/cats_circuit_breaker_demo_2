package com.github.morotsman
package presentation.demo

import cats.implicits._
import cats.MonadError
import cats.effect.{Ref, Temporal}

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
      override def mightFail(): F[Unit] = for {
        currentState <- state.get
        _ <- if (!currentState.isFailing) {
          Temporal[F].sleep(currentState.successLatencyInMillis.millis) >> MonadError[F, Throwable].unit
        } else {
          Temporal[F].sleep(currentState.requestTimeoutInMillis.millis) >> MonadError[F, Throwable].raiseError(new RuntimeException("Boooom"))
        }
      } yield ()

      override def toggleFailure(): F[Unit] =
        state.modify(s => (s.copy(isFailing = !s.isFailing), s))

      override def increaseSuccessLatency(): F[Unit] =
        state.modify(s => (s.copy(successLatencyInMillis = s.successLatencyInMillis * 2), s))

      override def increaseRequestTimeout(): F[Unit] =
        state.modify(s => (s.copy(requestTimeoutInMillis = s.requestTimeoutInMillis * 2), s))

      override def decreaseSuccessLatency(): F[Unit] =
        state.modify(s => (s.copy(successLatencyInMillis = s.successLatencyInMillis / 2), s))

      override def decreaseRequestTimeout(): F[Unit] =
        state.modify(s => (s.copy(requestTimeoutInMillis = s.requestTimeoutInMillis / 2), s))

      override def mayhemState(): F[MayhemState] =
        state.get
    }

}
