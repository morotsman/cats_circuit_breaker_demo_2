package com.github.morotsman
package presentation.slides.demo_slide.animations

import cats.implicits._
import cats.Monad
import cats.effect.{Ref, Temporal}
import presentation.demo.{CircuitBreakerState, SourceOfMayhem, Statistics}
import presentation.slides.demo_slide.DemoProgramExecutor
import presentation.tools.NConsole
import presentation.slides.demo_slide.animations.AnimationState.{AnimationMapper, AnimationState, CLOSED_FAILING, CLOSED_SUCCEED, UNKNOWN}
import presentation.slides.demo_slide.animations.ClosedFailure.ClosedFailureAnimation
import presentation.slides.demo_slide.animations.ClosedSuccess.ClosedSuccessAnimation
import presentation.slides.demo_slide.animations.Static.staticAnimation

import scala.concurrent.duration.DurationInt

object AnimationState extends Enumeration {
  type AnimationState = Value
  val UNKNOWN, CLOSED_SUCCEED, CLOSED_FAILING = Value

  val AnimationMapper = Map(
    UNKNOWN -> staticAnimation,
    CLOSED_SUCCEED -> ClosedSuccessAnimation,
    CLOSED_FAILING -> ClosedFailureAnimation
  )
}

final case class AnimatorState
(
  animationState: AnimationState
)

object AnimatorState {
  def make(): AnimatorState = AnimatorState(UNKNOWN)
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
        animationState = if (
          statisticsInfo.circuitBreakerState == CircuitBreakerState.CLOSED && !mayhemState.isFailing
        ) {
          CLOSED_SUCCEED
        } else if (
          statisticsInfo.circuitBreakerState == CircuitBreakerState.CLOSED && mayhemState.isFailing
        ) {
          CLOSED_FAILING
        } else {
          UNKNOWN
        }
        updated <- if (animatorState.animationState != animationState) {
          state.modify(s => (s.copy(
            animationState = animationState
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

      animate(0)
    }
  }
}





