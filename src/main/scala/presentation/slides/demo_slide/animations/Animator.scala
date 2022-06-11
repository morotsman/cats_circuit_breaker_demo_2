package com.github.morotsman
package presentation.slides.demo_slide.animations

import cats.effect.implicits._
import cats.implicits._
import cats.Monad
import cats.effect.{Fiber, Ref, Temporal}
import presentation.demo.{CircuitBreakerState, CircuitBreakerStateListener, OutcomeListener, SourceOfMayhem, Statistics}
import presentation.slides.demo_slide.{ControlPanel, DemoProgramExecutor, ExecutorStartedListener}
import presentation.tools.NConsole
import presentation.demo.CircuitBreakerState.CircuitBreakerState
import presentation.slides.demo_slide.animations.AnimationState.{AnimationState, _}

import cats.effect.std.Queue
import monocle.Lens
import monocle.macros.Lenses

import scala.concurrent.duration.DurationInt

@Lenses
final case class AnimatorState
(
  currentCircuitBreakerState: CircuitBreakerState,
  isStarted: Boolean,
  isFailing: Boolean,
  currentAnimationState: AnimationState
)

object AnimatorState {
  def make(): AnimatorState = AnimatorState(
    currentCircuitBreakerState = CircuitBreakerState.CLOSED,
    isStarted = false,
    isFailing = false,
    currentAnimationState = AnimationState.NOT_STARTED
  )
}

trait Animator[F[_]] extends CircuitBreakerStateListener[F] with ExecutorStartedListener[F] with OutcomeListener[F] {
  def animate(): F[Unit]

  def stop(): F[Unit]
}

sealed trait Event

case class AnimationEvent(
                           animationState: AnimationState
                         ) extends Event

case class TransitionEvent(
                            animationState: AnimationState
                          ) extends Event

case class PoisonPill() extends Event

case class CleanupCompleted() extends Event


object Animator {

  def make[F[_] : Temporal : NConsole]
  (
    state: Ref[F, AnimatorState],
    statistics: Statistics[F],
    sourceOfMayhem: SourceOfMayhem[F],
    demoProgramExecutor: DemoProgramExecutor[F],
    controlPanel: ControlPanel[F]
  ): F[Animator[F]] = {

    val isStarted: Lens[AnimatorState, Boolean] = AnimatorState.isStarted
    val isFailing = AnimatorState.isFailing
    val currentCircuitBreakerState = AnimatorState.currentCircuitBreakerState
    val currentAnimationState = AnimatorState.currentAnimationState

    for {
      queue <- Queue.unbounded[F, Event]
      cleanupCompleted <- Queue.unbounded[F, Event]
      animator =
        new Animator[F] {

          override def stop(): F[Unit] =
            queue.offer(PoisonPill()) >> cleanupCompleted.take.as(())

          def executorStarted(programStarted: Boolean): F[Unit] = for {
            animatorState <- state.get
            _ <- if (programStarted && animatorState.currentAnimationState != NOT_STARTED) {
              queue.offer(AnimationEvent(animatorState.currentAnimationState))
            } else if (programStarted && !animatorState.isFailing) {
              queue.offer(AnimationEvent(CLOSED_SUCCEED))
            } else if (programStarted && animatorState.isFailing) {
              queue.offer(AnimationEvent(CLOSED_FAILING))
            } else {
              Monad[F].unit
            }
            _ <- state.update(
              isStarted.replace(programStarted)
            )
          } yield ()

          override def outcomeUpdated(programIsFailing: Boolean): F[Unit] = for {
            animatorState <- state.get
            _ <- if (
              animatorState.currentCircuitBreakerState == CircuitBreakerState.CLOSED &&
                animatorState.isStarted &&
                animatorState.isFailing &&
                !programIsFailing
            ) {
              queue.offer(AnimationEvent(CLOSED_SUCCEED))
            } else if (
              animatorState.currentCircuitBreakerState == CircuitBreakerState.CLOSED &&
                animatorState.isStarted &&
                !animatorState.isFailing &&
                programIsFailing
            ) {
              queue.offer(AnimationEvent(CLOSED_FAILING))
            } else {
              Monad[F].unit
            }
            _ <- state.update(
              isFailing.replace(programIsFailing)
            )
          } yield ()

          override def circuitBreakerStateUpdated(circuitBreakerState: CircuitBreakerState): F[Unit] = for {
            animatorState <- state.get
            _ <- if (circuitBreakerState == CircuitBreakerState.CLOSED) {
              queue.offer(TransitionEvent(TRANSFER_HALF_OPEN_TO_CLOSED)) >>
                queue.offer(AnimationEvent(CLOSED_SUCCEED))
            } else if (circuitBreakerState == CircuitBreakerState.HALF_OPEN) {
              queue.offer(TransitionEvent(TRANSFER_OPEN_TO_HALF_OPEN)) >>
                queue.offer(AnimationEvent(HALF_OPEN))
            } else if (circuitBreakerState == CircuitBreakerState.OPEN && animatorState.currentCircuitBreakerState == CircuitBreakerState.HALF_OPEN) {
              queue.offer(TransitionEvent(TRANSFER_HALF_OPEN_TO_OPEN)) >>
                queue.offer(AnimationEvent(OPEN))
            } else {
              queue.offer(TransitionEvent(TRANSFER_CLOSED_TO_OPEN)) >> queue.offer(AnimationEvent(OPEN))
            }
            _ <- state.update(
              currentCircuitBreakerState.replace(circuitBreakerState)
            )
          } yield ()

          override def animate(): F[Unit] = {
            def loop(maybeCancelableAnimation: Option[Fiber[F, Throwable, Unit]]): F[Unit] = for {
              event <- queue.take
              _ <- maybeCancelableAnimation.traverse(_.cancel)
              _ <- event match {
                case PoisonPill() => cleanupCompleted.offer(CleanupCompleted())
                case AnimationEvent(animationState) =>
                  for {
                    _ <- state.update(
                      currentAnimationState.replace(animationState)
                    )
                    cancelable <- showStateAnimation(animationState).start
                    _ <- loop(Option(cancelable))
                  } yield ()
                case TransitionEvent(animationState) =>
                  for {
                    _ <- showTransitionAnimation(animationState)
                    _ <- loop(None)
                  } yield ()
              }
            } yield ()

            for {
              s <- state.get
              _ <- queue.offer(AnimationEvent(s.currentAnimationState))
              _ <- loop(None)
            } yield ()
          }

          def showStateAnimation(animationState: AnimationState): F[Unit] =
            showAnimation(animationState, delay = 500, delayAccelerator = 1.1, forever = true)

          def showTransitionAnimation(animationState: AnimationState): F[Unit] =
            showAnimation(animationState, delay = 160, delayAccelerator = 1.2, forever = false) >>
              Temporal[F].sleep(150.milli)

          def showAnimation
          (
            animationState: AnimationState,
            delay: Double,
            delayAccelerator: Double,
            forever: Boolean
          ): F[Unit] = {

            def loop(frame: Int, frameDelay: Double): F[Unit] = {
              for {
                demoProgramExecutorState <- demoProgramExecutor.getState
                statisticsInfo <- statistics.getStatisticsInfo
                controlPanel <- controlPanel.getState()
                mayhemState <- sourceOfMayhem.mayhemState()
                animation = AnimationMapper(animationState)
                _ <- NConsole[F].clear()
                _ <- NConsole[F].writeString(
                  animation(frame)(
                    statisticsInfo,
                    controlPanel.input,
                    mayhemState,
                    demoProgramExecutorState.circuitBreakerConfiguration,
                    demoProgramExecutorState.isStarted
                  ))
                _ <- Temporal[F].sleep(frameDelay.toInt.milli)
                nextFrame = frame + 1
                _ <- if (!forever && nextFrame == animation.size) {
                  Monad[F].unit
                } else if (nextFrame == animation.size) {
                  loop(
                    frame = 0,
                    frameDelay = delay
                  )
                } else {
                  loop(
                    frame = nextFrame,
                    frameDelay = frameDelay / delayAccelerator,
                  )
                }
              } yield ()
            }

            loop(frame = 0, frameDelay = delay)
          }

        }
      _ <- demoProgramExecutor.registerExecutorListener(animator)
      _ <- statistics.registerCircuitBreakerStateListener(animator)
      _ <- sourceOfMayhem.registerOutcomeListener(animator)
    }

    yield animator
  }


}





