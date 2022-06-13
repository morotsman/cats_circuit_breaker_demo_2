package com.github.morotsman
package presentation.tools

import cats.effect.{Ref, Sync}
import cats.implicits._

case class SimpleSlideState()

object SimpleSlide {
  def apply[F[_] : Sync : NConsole](slideContent: String): F[Slide[F]] =
    Ref.of[F, SimpleSlideState](SimpleSlideState()).map { ref =>
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
    }
}
