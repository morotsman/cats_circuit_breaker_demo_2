package com.github.morotsman
package presentation.tools.transition

import presentation.tools.Slide

trait Transition[F[_]] {
  def transition(from: Slide[F], to: Slide[F]): F[Unit]
}
