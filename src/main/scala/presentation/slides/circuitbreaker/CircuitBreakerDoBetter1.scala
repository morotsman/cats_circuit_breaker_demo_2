package com.github.morotsman
package presentation.slides.circuitbreaker

import presentation.tools.{Input, NConsole, SimpleSlide, Slide}

import cats.effect.Sync

case class CircuitBreakerDoBetter1[F[_] : Sync : NConsole]() extends SimpleSlide[F] {
  val content =
    Sync[F].pure("""
      |
      |
      |  ___                             _       _         _   _          ___
      | / __|__ _ _ _   __ __ _____   __| |___  | |__  ___| |_| |_ ___ _ |__ \
      || (__/ _` | ' \  \ V  V / -_) / _` / _ \ | '_ \/ -_)  _|  _/ -_) '_|/_/
      | \___\__,_|_||_|  \_/\_/\___| \__,_\___/ |_.__/\___|\__|\__\___|_| (_)
      |
      |
      |
      |""".stripMargin)

}
