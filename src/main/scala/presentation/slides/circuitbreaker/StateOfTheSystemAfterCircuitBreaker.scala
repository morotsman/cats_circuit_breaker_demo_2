package com.github.morotsman
package presentation.slides.circuitbreaker

import presentation.tools.{Input, NConsole, Slide}

import cats.effect.Sync

case class StateOfTheSystemAfterCircuitBreaker[F[_] : Sync : NConsole]() extends Slide[F] {
  val text =
    """
      |
      |   Show how the system looks after the circuit breaker has been applied
      |
      |
      |""".stripMargin

  override def show(): F[Unit] = NConsole[F].writeString(text)

  override def userInput(input: Input): F[Unit] = Sync[F].unit
}
