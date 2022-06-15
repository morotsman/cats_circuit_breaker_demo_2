package com.github.morotsman
package presentation.tools.transition
import presentation.tools.Slide

import cats.effect.kernel.Temporal
import cats.{Monad}

object Nothing {
  def apply[F[_]: Temporal](): Transition[F] = new Transition[F] {
    override def transition(from: Slide[F], to: Slide[F]): F[Unit] =
      Monad[F].unit
  }
}
