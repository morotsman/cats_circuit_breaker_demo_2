package com.github.morotsman
package presentation.slides

import presentation.tools.{Input, NConsole, Slide}

import cats.effect.Sync

case class References[F[_] : Sync : NConsole]() extends Slide[F] {
  override def show(): F[Unit] =
    NConsole[F].writeStringCenterAligned(
      """
        |
        |
        |                         ___      __
        |                        | _ \___ / _|___ _ _ ___ _ _  __ ___ ___
        |                        |   / -_)  _/ -_) '_/ -_) ' \/ _/ -_|_-<
        |                        |_|_\___|_| \___|_| \___|_||_\__\___/__/
        |
        |
        |
        |
        |  Release it! Design and Deploy Production-Ready Software (Pragmatic Programmers) by Michael T. Nygard
        |
        |  Building Microservices: Designing Fine-Grained Systems (O'Reilly) by Sam Newman
        |
        |  Article by Martin Fowler: https://martinfowler.com/bliki/CircuitBreaker.html
        |""".stripMargin)

  override def userInput(input: Input): F[Unit] = Sync[F].unit

}
