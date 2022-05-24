package com.github.morotsman
package presentation.slides.demo_slide.animations

import cats.implicits._
import cats.Monad
import cats.effect.{Ref, Temporal}
import presentation.demo.{CircuitBreakerState, MayhemState, SourceOfMayhem, Statistics, StatisticsInfo}
import presentation.slides.demo_slide.{CircuitBreakerConfiguration, DemoProgramExecutor, DemoProgramExecutorState}
import presentation.tools.{Input, NConsole}
import presentation.demo.CircuitBreakerState.CircuitBreakerState
import presentation.slides.demo_slide.animations.AnimationState._

import scala.concurrent.duration.DurationInt


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
            statisticsInfo.circuitBreakerState == CircuitBreakerState.CLOSED && (animatorState.lastCircuitBreakerState == CircuitBreakerState.HALF_OPEN || animatorState.lastCircuitBreakerState == CircuitBreakerState.OPEN)
          ) {
            TRANSFER_HALF_OPEN_TO_CLOSED
          } else if (
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
          } else if (
            statisticsInfo.circuitBreakerState == CircuitBreakerState.OPEN && animatorState.lastCircuitBreakerState == CircuitBreakerState.HALF_OPEN
          ) {
            TRANSFER_HALF_OPEN_TO_OPEN
          } else if (
            statisticsInfo.circuitBreakerState == CircuitBreakerState.OPEN
          ) {
            OPEN
          } else if (
            statisticsInfo.circuitBreakerState == CircuitBreakerState.HALF_OPEN && animatorState.lastCircuitBreakerState == CircuitBreakerState.OPEN
          ) {
            TRANSFER_OPEN_TO_HALF_OPEN
          } else if (
            statisticsInfo.circuitBreakerState == CircuitBreakerState.HALF_OPEN
          ) {
            HALF_OPEN
          } else {
            UNKNOWN
          }
        }
        _ <- if (
          animationState == TRANSFER_CLOSED_TO_OPEN ||
            animationState == TRANSFER_OPEN_TO_HALF_OPEN ||
            animationState == TRANSFER_HALF_OPEN_TO_CLOSED ||
            animationState == TRANSFER_HALF_OPEN_TO_OPEN
        ) {
          for {
            _ <- if (animationState == TRANSFER_HALF_OPEN_TO_CLOSED) { // Seems like the Circuit Breaker impl I use goes directly from OPEN to CLOSED, or maybe it report the state wrong?
              showTransferAnimation(
                AnimationMapper(TRANSFER_OPEN_TO_HALF_OPEN),
                statisticsInfo,
                mayhemState,
                demoProgramExecutorState
              )
            } else Monad[F].unit
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
            _ <-
              NConsole[F].clear() >>
                NConsole[F].writeString(
                  animation(frameToShow)(
                    statisticsInfo,
                    statisticsInfo.currentInput,
                    mayhemState,
                    demoProgramExecutorState.circuitBreakerConfiguration,
                    demoProgramExecutorState.isStarted
                  )) >>
                Temporal[F].sleep(500.milli) >>
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
      NConsole[F].clear() >>
        NConsole[F].writeString(
          animation(frame)(
            statisticsInfo,
            statisticsInfo.currentInput,
            mayhemState,
            demoProgramExecutorState.circuitBreakerConfiguration,
            demoProgramExecutorState.isStarted
          )) >>
        Temporal[F].sleep(80.milli) >>
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





