package com.github.morotsman
package presentation.tools

import cats.effect.Sync

abstract class SimpleSlide[F[_] : Sync : NConsole]() extends Slide[F] {
  def content: String

  override def startShow(): F[Unit] = {
    NConsole[F].writeStringCenterAligned(content)
  }

  override def userInput(input: Input): F[Unit] = Sync[F].unit
}
