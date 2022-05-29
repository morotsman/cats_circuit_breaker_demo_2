package com.github.morotsman
package presentation.slides.circuitbreaker

import presentation.tools.{Input, NConsole, Slide}

import cats.effect.Sync

case class CircuitBreakerDoBetter1[F[_] : Sync : NConsole]() extends Slide[F] {
  val text =
    """
      |
      |
      |  ___                             _       _         _   _          ___
      | / __|__ _ _ _   __ __ _____   __| |___  | |__  ___| |_| |_ ___ _ |__ \
      || (__/ _` | ' \  \ V  V / -_) / _` / _ \ | '_ \/ -_)  _|  _/ -_) '_|/_/
      | \___\__,_|_||_|  \_/\_/\___| \__,_\___/ |_.__/\___|\__|\__\___|_| (_)
      |
      |
      |
      |""".stripMargin

  override def show(): F[Unit] = NConsole[F].writeStringCenterAligned(text)

  override def userInput(input: Input): F[Unit] = Sync[F].unit
}
