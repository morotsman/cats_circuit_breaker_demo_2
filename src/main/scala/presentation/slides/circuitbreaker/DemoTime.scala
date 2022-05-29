package com.github.morotsman
package presentation.slides.circuitbreaker

import presentation.tools.{Input, NConsole, Slide}

import cats.effect.Sync

case class DemoTime[F[_] : Sync : NConsole]() extends Slide[F] {
  val text =
    """
      |
      |
      |  ___                  _   _           _
      | |   \ ___ _ __  ___  | |_(_)_ __  ___| |
      | | |) / -_) '  \/ _ \ |  _| | '  \/ -_)_|
      | |___/\___|_|_|_\___/  \__|_|_|_|_\___(_)
      |
      |
      |
      |
      |""".stripMargin

  override def show(): F[Unit] = NConsole[F].writeStringCenterAligned(text)

  override def userInput(input: Input): F[Unit] = Sync[F].unit
}
