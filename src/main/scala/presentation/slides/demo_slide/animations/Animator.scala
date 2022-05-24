package com.github.morotsman
package presentation.slides.demo_slide.animations

import cats.implicits._
import cats.Monad
import cats.effect.{Ref, Temporal}
import presentation.demo.{CircuitBreakerState, MayhemState, SourceOfMayhem, Statistics, StatisticsInfo}
import presentation.slides.demo_slide.{CircuitBreakerConfiguration, DemoProgramExecutor, DemoProgramExecutorState, animations}
import presentation.tools.{Input, NConsole}
import presentation.slides.demo_slide.animations.AnimationState.{AnimationMapper, AnimationState, CLOSED_FAILING, CLOSED_SUCCEED, NOT_STARTED, TRANSFER_CLOSED_TO_OPEN, UNKNOWN}
import presentation.slides.demo_slide.animations.ClosedFailure.ClosedFailureAnimation
import presentation.slides.demo_slide.animations.ClosedSuccess.ClosedSuccessAnimation
import presentation.slides.demo_slide.animations.Static.staticAnimation

import com.github.morotsman.presentation.demo.CircuitBreakerState.CircuitBreakerState

import scala.concurrent.duration.DurationInt

object AnimationState extends Enumeration {
  type AnimationState = Value
  val NOT_STARTED, UNKNOWN, CLOSED_SUCCEED, CLOSED_FAILING, TRANSFER_CLOSED_TO_OPEN, OPEN, TRANSFER_OPEN_TO_HALF_OPEN, HALF_OPEN, TRANSFER_HALF_OPEN_TO_OPEN, TRANSFER_HALF_OPEN_TO_CLOSED = Value

  val AnimationMapper = Map(
    NOT_STARTED -> staticAnimation,
    UNKNOWN -> staticAnimation,
    CLOSED_SUCCEED -> ClosedSuccessAnimation,
    CLOSED_FAILING -> ClosedFailureAnimation,
    TRANSFER_CLOSED_TO_OPEN -> TransferClosedToOpen.animation
  )
}

final case class AnimatorState
(
  animationState: AnimationState,
  lastCircuitBreakerState: CircuitBreakerState
)

object AnimatorState {
  def make(): AnimatorState = AnimatorState(
    animationState = UNKNOWN,
    lastCircuitBreakerState = CircuitBreakerState.CLOSED
  )
}

trait Animator[F[_]] {
  def animate(): F[Unit]
}

object Animator {

  def make[F[_] : Temporal : NConsole]
  (
    state: Ref[F, AnimatorState],
    statistics: Statistics[F],
    sourceOfMayhem: SourceOfMayhem[F],
    demoProgramExecutor: DemoProgramExecutor[F],
  ): Animator[F] = new Animator[F] {


    override def animate(): F[Unit] = {
      def animate(frame: Int): F[Unit] = for {
        animatorState <- state.get
        demoProgramExecutorState <- demoProgramExecutor.getState()
        statisticsInfo <- statistics.getStatisticsInfo()
        mayhemState <- sourceOfMayhem.mayhemState()
        animationState = if (!demoProgramExecutorState.isStarted) {
          NOT_STARTED
        } else {
          if (
            statisticsInfo.circuitBreakerState == CircuitBreakerState.CLOSED && !mayhemState.isFailing && demoProgramExecutorState.isStarted
          ) {
            CLOSED_SUCCEED
          } else if (
            statisticsInfo.circuitBreakerState == CircuitBreakerState.CLOSED && mayhemState.isFailing
          ) {
            CLOSED_FAILING
          } else if (
            statisticsInfo.circuitBreakerState == CircuitBreakerState.OPEN && animatorState.lastCircuitBreakerState == CircuitBreakerState.CLOSED
          ) {
            TRANSFER_CLOSED_TO_OPEN
          } else {
            UNKNOWN
          }
        }
        _ <- if (animationState == TRANSFER_CLOSED_TO_OPEN) {
          for {
            _ <- showTransferAnimation(
              AnimationMapper(animationState),
              statisticsInfo,
              mayhemState,
              demoProgramExecutorState
            )
            _ <- state.modify(s => (s.copy(
              lastCircuitBreakerState = statisticsInfo.circuitBreakerState
            ), s))
            _ <- NConsole[F].clear()
            _ <- animate(0)
          } yield ()
        } else {
          for {
            updated <- if (animatorState.animationState != animationState) {
              state.modify(s => (s.copy(
                animationState = animationState,
                lastCircuitBreakerState = statisticsInfo.circuitBreakerState
              ), s)).as(true)
            } else {
              Monad[F].pure(false)
            }
            frameToShow = if (updated) 0 else frame
            animation = AnimationMapper(animationState)
            _ <- NConsole[F].writeString(
              animation(frameToShow)(
                statisticsInfo,
                statisticsInfo.currentInput,
                mayhemState,
                demoProgramExecutorState.circuitBreakerConfiguration,
                demoProgramExecutorState.isStarted
              )) >>
              Temporal[F].sleep(500.milli) >>
              NConsole[F].clear() >>
              animate(if (frameToShow < animation.size - 1) frameToShow + 1 else 0)
          } yield ()
        }
      } yield ()

      animate(0)
    }

    def showTransferAnimation
    (
      animation: List[(StatisticsInfo, Option[Input], MayhemState, CircuitBreakerConfiguration, Boolean) => String],
      statisticsInfo: StatisticsInfo,
      mayhemState: MayhemState,
      demoProgramExecutorState: DemoProgramExecutorState[F],
      frame: Int = 0
    ): F[Unit] = if (frame >= animation.size) {
      Monad[F].unit
    } else {
      Temporal[F].sleep(80.milli) >>
        NConsole[F].clear() >>
        NConsole[F].writeString(
          animation(frame)(
            statisticsInfo,
            statisticsInfo.currentInput,
            mayhemState,
            demoProgramExecutorState.circuitBreakerConfiguration,
            demoProgramExecutorState.isStarted
          )) >>
        showTransferAnimation(
          animation,
          statisticsInfo,
          mayhemState,
          demoProgramExecutorState,
          frame + 1
        )
    }

  }


}





