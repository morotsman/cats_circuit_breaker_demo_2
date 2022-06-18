package com.github.morotsman
package presentation.tools

import cats.effect.Sync
import cats.implicits._

object SimpleSlide {
  def apply[F[_] : Sync : NConsole](slideContent: String): Slide[F] =
    new Slide[F] {
      override def content: F[String] =
        Sync[F].pure(slideContent)

      override def startShow(): F[Unit] =
        content >>= (NConsole[F].writeStringCenterAligned(_))

      override def stopShow(): F[Unit] =
        Sync[F].unit

      override def userInput(input: Input): F[Unit] =
        Sync[F].unit
    }

  implicit class ToSimpleSlide(val s: String) {
    def toSlide[F[_]: Sync : NConsole]: Slide[F] = SimpleSlide[F](s)
  }

}
