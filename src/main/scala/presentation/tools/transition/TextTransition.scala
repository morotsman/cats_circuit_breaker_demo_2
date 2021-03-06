package com.github.morotsman
package presentation.tools.transition

import presentation.tools.{NConsole, Slide}

import cats.implicits._
import cats.effect.kernel.Temporal

import scala.concurrent.duration.DurationInt

object TextTransition {
  def apply[F[_]: Temporal: NConsole](toWrite: String): Transition[F] = new Transition[F] {
    override def transition(from: Slide[F], to: Slide[F]): F[Unit] =
      NConsole[F].writeStringCenterAligned(toWrite) >> Temporal[F].sleep(1.seconds)
  }
}
