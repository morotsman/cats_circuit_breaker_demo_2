package com.github.morotsman
package presentation.slides.demo_slide

import presentation.tools.{Input, NConsole, Slide}

import cats.implicits._
import cats.effect.implicits._
import cats.effect.Temporal
import presentation.demo.{SourceOfMayhem, Statistics}
import presentation.slides.demo_slide.animations.Animator

final case class CircuitBreakerSlide[F[_] : Temporal]
(
  sourceOfMayhem: SourceOfMayhem[F],
  statistics: Statistics[F],
  demoProgramExecutor: DemoProgramExecutor[F],
  controlPanel: ControlPanel[F],
  animator: Animator[F]
) extends Slide[F] {

  override def show(): F[Unit] =
    (demoProgramExecutor.execute(), statistics.aggregate())
      .parTupled.background.use { _ =>
      animator.animate()
    }

  override def userInput(input: Input): F[Unit] =
    controlPanel.userInput(input)

}
