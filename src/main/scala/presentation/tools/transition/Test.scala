package com.github.morotsman
package presentation.tools.transition

import presentation.tools.{NConsole, Slide}

import cats.implicits._
import cats.effect.kernel.Temporal

import scala.concurrent.duration.DurationInt

object Test {
  def apply[F[_]: Temporal: NConsole](): Transition[F] = new Transition[F] {
    override def transition(from: Slide[F], to: Slide[F]): F[Unit] =
      NConsole[F].writeStringCenterAligned("hepp") >> Temporal[F].sleep(1.seconds)
  }
}
