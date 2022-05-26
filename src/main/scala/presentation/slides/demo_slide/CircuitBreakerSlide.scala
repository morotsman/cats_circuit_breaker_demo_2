package com.github.morotsman
package presentation.slides.demo_slide

import presentation.tools.{Input, NConsole, Slide}

import cats.implicits._
import cats.effect.implicits._
import cats.effect.{Ref, Temporal}
import presentation.demo.{MayhemState, SourceOfMayhem, Statistics, StatisticsState}
import presentation.slides.demo_slide.animations.{Animator, Animator2, AnimatorState, AnimatorState2}

object CircuitBreakerSlide {
  def make[F[_] : Temporal : NConsole](): F[Slide[F]] = for {
    sourceOfMayhem <- Ref[F].of(MayhemState.make()).map(SourceOfMayhem.make[F])
    statistics <- Ref[F].of(StatisticsState.make[F]()).map(Statistics.make[F])
    demoProgramExecutor <- Ref[F].of(DemoProgramExecutorState.make[F]()).flatMap(DemoProgramExecutor.make(
      _,
      sourceOfMayhem,
      statistics
    ))
    controlPanel <- Ref[F].of(ControlPanelState.make[F]()).map(ControlPanel.make[F](
      _,
      sourceOfMayhem,
      demoProgramExecutor,
      statistics
    ))
    animator <- Ref[F].of(AnimatorState2.make()).flatMap(
      Animator2.make[F](_, statistics, sourceOfMayhem, demoProgramExecutor)
    )

    slide = new Slide[F] {
      override def show(): F[Unit] =
        (demoProgramExecutor.execute(), statistics.aggregate()).parTupled.background.use { _ =>
          animator.animate()
        }

      override def userInput(input: Input): F[Unit] =
        controlPanel.userInput(input)
    }
  } yield slide

}

