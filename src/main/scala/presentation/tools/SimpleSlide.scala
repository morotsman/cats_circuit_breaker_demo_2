package com.github.morotsman
package presentation.tools

import cats.effect.Sync
import cats.implicits._

abstract class SimpleSlide[F[_] : Sync : NConsole]() extends Slide[F] {
  def content: F[String]

  override def startShow(): F[Unit] = {
    content >>= (NConsole[F].writeStringCenterAligned(_))
  }

  override def stopShow(): F[Unit] = Sync[F].unit

  override def userInput(input: Input): F[Unit] = Sync[F].unit
}
