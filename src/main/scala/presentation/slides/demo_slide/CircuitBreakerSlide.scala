package com.github.morotsman
package presentation.slides.demo_slide

import presentation.tools.{Input, Key, NConsole, Slide, SpecialKey}

import cats.implicits._
import cats.effect.implicits._
import cats.effect.{Ref, Sync, Temporal}
import presentation.demo.{Listeners, MayhemState, SourceOfMayhem, Statistics, StatisticsState}
import presentation.slides.demo_slide.animations.{Animator, AnimatorState}

object CircuitBreakerSlide {
  def make[F[_] : Temporal : NConsole](): F[Slide[F]] = for {
    sourceOfMayhem <- (Ref[F].of(MayhemState.make()), Ref[F].of(Listeners.make[F]())).mapN(SourceOfMayhem.make[F])
    statistics <- Ref[F].of(StatisticsState.make[F]()).map(Statistics.make[F])
    demoProgramExecutor <- Ref[F].of(DemoProgramExecutorState.make[F]()).flatMap(DemoProgramExecutor.make(
      _,
      sourceOfMayhem,
      statistics
    ))
    controlPanel <- Ref[F].of(ControlPanelState.make[F]()).map(ControlPanel.make[F](
      _,
      sourceOfMayhem,
      demoProgramExecutor
    ))
    animator <- Ref[F].of(AnimatorState.make()).flatMap(
      Animator.make[F](_, statistics, sourceOfMayhem, demoProgramExecutor, controlPanel)
    )

    slide = new Slide[F] {
      override def startShow(): F[Unit] =
        (demoProgramExecutor.execute(), statistics.aggregate()).parTupled.background.use { _ =>
          animator.animate()
        }

      override def stopShow(): F[Unit] =
        animator.stop()

      override def userInput(input: Input): F[Unit] =
        controlPanel.userInput(input)

      override def content: F[String] = animator.snapshot()
    }
  } yield slide

}

