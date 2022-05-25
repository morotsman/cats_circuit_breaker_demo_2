package com.github.morotsman
package presentation.slides.circuitbreaker

import presentation.tools.{Input, NConsole, Slide}

import cats.effect.Sync

case class InTheCode[F[_] : Sync : NConsole]() extends Slide[F] {
  val text =
    """
      |
      |   Show example how to use a circuit breaker in the code!
      |
      |
      |""".stripMargin

  override def show(): F[Unit] = NConsole[F].writeString(text)

  override def userInput(input: Input): F[Unit] = Sync[F].unit
}
