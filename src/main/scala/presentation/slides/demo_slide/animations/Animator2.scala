package com.github.morotsman
package presentation.slides.demo_slide.animations

import cats.effect.implicits._
import cats.implicits._
import cats.Monad
import cats.effect.{Fiber, Ref, Spawn, Temporal}
import presentation.demo.{CircuitBreakerState, CircuitBreakerStateListener, SourceOfMayhem, Statistics, StatisticsInfo, StatisticsListener}
import presentation.slides.demo_slide.DemoProgramExecutor
import presentation.tools.NConsole
import presentation.demo.CircuitBreakerState.CircuitBreakerState
import presentation.slides.demo_slide.animations.AnimationState.{AnimationState, _}

import cats.effect.std.Queue

import scala.concurrent.duration.DurationInt

final case class AnimatorState2
(
  currentCircuitBreakerState: CircuitBreakerState,
  isStarted: Boolean,
  isFailing: Boolean
)

object AnimatorState2 {
  def make(): AnimatorState2 = AnimatorState2(
    currentCircuitBreakerState = CircuitBreakerState.CLOSED,
    isStarted = false,
    isFailing = false
  )
}

trait Animator2[F[_]] extends StatisticsListener[F] with CircuitBreakerStateListener[F] {
  def animate(): F[Unit]

  def stop(): F[Unit]
}

case class Event[F[_]]
(
  isTransferAnimation: Boolean,
  animation: F[Unit]
)

object Animator2 {

  def make[F[_] : Temporal : NConsole : Spawn]
  (
    state: Ref[F, AnimatorState2],
    statistics: Statistics[F],
    sourceOfMayhem: SourceOfMayhem[F],
    demoProgramExecutor: DemoProgramExecutor[F],
  ): F[Animator2[F]] =
    for {
      queue <- Queue.unbounded[F, Event[F]]
      animator =
        new Animator2[F] {

          override def stop(): F[Unit] =
            queue.offer(Event[F](true, Monad[F].unit))

          override def statisticsUpdated(statisticsInfo: StatisticsInfo): F[Unit] = for {
            animatorState <- state.get
            demoProgramExecutorState <- demoProgramExecutor.getState()

            _ <- if (!animatorState.isStarted && !animatorState.isFailing && demoProgramExecutorState.isStarted) {
              queue.offer(Event[F](false, showStateAnimation(CLOSED_SUCCEED))) >> state.modify(s => (s.copy(
                isStarted = true
              ), s))
            } else if (!animatorState.isStarted && animatorState.isFailing && demoProgramExecutorState.isStarted) {
              queue.offer(Event[F](false, showStateAnimation(CLOSED_FAILING))) >> state.modify(s => (s.copy(
                isStarted = true
              ), s))
            } else if (animatorState.isStarted && !demoProgramExecutorState.isStarted) {
              queue.offer(Event[F](false, showStateAnimation(NOT_STARTED))) >> state.modify(s => (s.copy(
                isStarted = false
              ), s))
            } else {
              Monad[F].unit
            }

            mayhemState <- sourceOfMayhem.mayhemState()
            _ <- if (
              animatorState.currentCircuitBreakerState == CircuitBreakerState.CLOSED &&
                animatorState.isStarted &&
                animatorState.isFailing &&
                !mayhemState.isFailing
            ) {
              queue.offer(Event[F](false, showStateAnimation(CLOSED_SUCCEED))) >> state.modify(s => (s.copy(
                isFailing = false
              ), s))
            } else if (
              animatorState.currentCircuitBreakerState == CircuitBreakerState.CLOSED &&
                animatorState.isStarted &&
                !animatorState.isFailing &&
                mayhemState.isFailing
            ) {
              queue.offer(Event[F](false, showStateAnimation(CLOSED_FAILING))) >> state.modify(s => (s.copy(
                isFailing = true
              ), s))
            } else {
              Monad[F].unit
            }
          } yield ()

          override def circuitBreakerStateUpdated(circuitBreakerState: CircuitBreakerState): F[Unit] = for {
            animatorState <- state.get
            _ <- if (circuitBreakerState == CircuitBreakerState.CLOSED) {
              queue.offer(Event[F](true, showTransitionAnimation(TRANSFER_HALF_OPEN_TO_CLOSED))) >>
                queue.offer(Event[F](false, showStateAnimation(CLOSED_SUCCEED)))
            } else if (circuitBreakerState == CircuitBreakerState.HALF_OPEN) {
              queue.offer(Event[F](true, showTransitionAnimation(TRANSFER_OPEN_TO_HALF_OPEN))) >>
                queue.offer(Event[F](false, showStateAnimation(HALF_OPEN)))
            } else if (circuitBreakerState == CircuitBreakerState.OPEN && animatorState.currentCircuitBreakerState == CircuitBreakerState.HALF_OPEN) {
              queue.offer(Event[F](true, showTransitionAnimation(TRANSFER_HALF_OPEN_TO_OPEN))) >>
                queue.offer(Event[F](false, showStateAnimation(OPEN)))
            } else {
              queue.offer(Event[F](true, showTransitionAnimation(TRANSFER_CLOSED_TO_OPEN))) >>
                queue.offer(Event[F](false, showStateAnimation(OPEN)))
            }
            _ <- state.modify(s => (s.copy(
              currentCircuitBreakerState = circuitBreakerState
            ), s))
          } yield ()

          override def animate(): F[Unit] = {
            def loop(maybeCancelableAnimation: Option[Fiber[F, Throwable, Unit]]): F[Unit] = for {
              event <- queue.take
              _ <- maybeCancelableAnimation.traverse(_.cancel)
              maybeCancelable <- if (event.isTransferAnimation)
                event.animation.as(None)
              else
                event.animation.start.map(Some(_))
              _ <- loop(maybeCancelable)
            } yield ()

            for {
              _ <- queue.offer(Event[F](false, showStateAnimation(NOT_STARTED)))
              _ <- loop(None)
            } yield ()

          }

          def showStateAnimation(animationState: AnimationState, frame: Int = 0, delay: Double = 500): F[Unit] = for {
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
            _ <- Temporal[F].sleep(delay.toInt.milli)
            _ <- showStateAnimation(
              animationState,
              frame = if (frame + 1 == animation.size) 0 else frame + 1,
              delay = if (frame + 1 == animation.size) 500 else delay / 1.1
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





