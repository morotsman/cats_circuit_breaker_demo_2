package com.github.morotsman
package presentation.slides.demo_slide.animations

import cats.effect.implicits._
import cats.implicits._
import cats.Monad
import cats.effect.{Fiber, Ref, Spawn, Temporal}
import presentation.demo.{CircuitBreakerState, CircuitBreakerStateListener, MayhemState, SourceOfMayhem, Statistics, StatisticsInfo, StatisticsListener}
import presentation.slides.demo_slide.{CircuitBreakerConfiguration, DemoProgramExecutor, DemoProgramExecutorState}
import presentation.tools.{Input, NConsole}
import presentation.demo.CircuitBreakerState.{CLOSED, CircuitBreakerState}
import presentation.slides.demo_slide.animations.AnimationState.{AnimationState, _}

import cats.effect.std.Queue

import scala.concurrent.duration.DurationInt

final case class AnimatorState2
(
  lastCircuitBreakerState: CircuitBreakerState
)

object AnimatorState2 {
  def make(): AnimatorState2 = AnimatorState2(
    lastCircuitBreakerState = CircuitBreakerState.CLOSED
  )
}

trait Animator2[F[_]] extends StatisticsListener[F] with CircuitBreakerStateListener[F] {
  def animate(): F[Unit]
}

object Animator2 {

  def make[F[_] : Temporal : NConsole : Spawn]
  (
    state: Ref[F, AnimatorState2],
    statistics: Statistics[F],
    sourceOfMayhem: SourceOfMayhem[F],
    demoProgramExecutor: DemoProgramExecutor[F],
  ): F[Animator2[F]] =
    for {
      queue <- Queue.unbounded[F, (Boolean, F[Unit])]
      animator =
        new Animator2[F] {

          override def statisticsUpdated(statisticsInfo: StatisticsInfo): F[Unit] =
            Monad[F].unit

          override def circuitBreakerStateUpdated(circuitBreakerState: CircuitBreakerState): F[Unit] = for {
            animatorState <- state.get
            _ <- if (circuitBreakerState == CircuitBreakerState.CLOSED) {
              queue.offer((true, showTransitionAnimation(TRANSFER_HALF_OPEN_TO_CLOSED))) >>
                queue.offer((false, showStateAnimation(CLOSED_SUCCEED)))
            } else if (circuitBreakerState == CircuitBreakerState.HALF_OPEN) {
              queue.offer((true, showTransitionAnimation(TRANSFER_OPEN_TO_HALF_OPEN))) >>
                queue.offer((false, showStateAnimation(HALF_OPEN)))
            } else if (circuitBreakerState == CircuitBreakerState.OPEN && animatorState.lastCircuitBreakerState == CircuitBreakerState.HALF_OPEN) {
              queue.offer((true, showTransitionAnimation(TRANSFER_HALF_OPEN_TO_OPEN))) >>
                queue.offer((false, showStateAnimation(OPEN)))
            } else {
              queue.offer((true, showTransitionAnimation(TRANSFER_CLOSED_TO_OPEN))) >>
                queue.offer((false, showStateAnimation(OPEN)))
            }
            _ <- state.modify(s => (s.copy(
              lastCircuitBreakerState = circuitBreakerState
            ), s))
          } yield ()

          override def animate(): F[Unit] = {
            def loop(maybeCancelableAnimation: Option[Fiber[F, Throwable, Unit]]): F[Unit] = for {
              (transfer, animation) <- queue.take
              _ <- maybeCancelableAnimation.traverse(_.cancel)
              maybeCancelable <- if (transfer)
                animation.as(None)
              else
                animation.start.map(Some(_))
              _ <- loop(maybeCancelable)
            } yield ()

            for {
              _ <- queue.offer((false, showStateAnimation(NOT_STARTED)))
              _ <- loop(None)
            } yield ()

          }

          def showStateAnimation(animationState: AnimationState, frame: Int = 0): F[Unit] = for {
            demoProgramExecutorState <- demoProgramExecutor.getState()
            statisticsInfo <- statistics.getStatisticsInfo()
            mayhemState <- sourceOfMayhem.mayhemState()
            animation <- Monad[F].pure(AnimationMapper(animationState))
            _ <- NConsole[F].clear() >>
              NConsole[F].writeString(
                animation(frame)(
                  statisticsInfo,
                  statisticsInfo.currentInput,
                  mayhemState,
                  demoProgramExecutorState.circuitBreakerConfiguration,
                  demoProgramExecutorState.isStarted
                ))
            _ <- Temporal[F].sleep(500.milli)
            _ <- showStateAnimation(
              animationState,
              frame = if (frame + 1 == animation.size) 0 else frame + 1
            )
          } yield ()

          def showTransitionAnimation
          (
            animationState: AnimationState,
            frame: Int = 0,
            delay: Double = 160
          ): F[Unit] = if (frame >= AnimationMapper(animationState).size) {
            Monad[F].unit
          } else {
            for {
              demoProgramExecutorState <- demoProgramExecutor.getState()
              statisticsInfo <- statistics.getStatisticsInfo()
              mayhemState <- sourceOfMayhem.mayhemState()
              animation = AnimationMapper(animationState)
              _ <- NConsole[F].clear()
              _ <- NConsole[F].writeString(
                animation(frame)(
                  statisticsInfo,
                  statisticsInfo.currentInput,
                  mayhemState,
                  demoProgramExecutorState.circuitBreakerConfiguration,
                  demoProgramExecutorState.isStarted
                ))
              _ <- Temporal[F].sleep(delay.toInt.milli)
              _ <- showTransitionAnimation(
                animationState,
                frame + 1,
                delay = delay / 1.1
              )
            } yield ()
          }
        }
      _ <- statistics.registerStatisticsListener(animator)
      _ <- statistics.registerCircuitBreakerStateListener(animator)
    }

    yield animator


}





