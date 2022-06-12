package com.github.morotsman
package presentation.tools

import cats._
import cats.effect._
import cats.effect.implicits._
import cats.implicits._

import scala.concurrent.duration.DurationInt

trait Presentation[F[_]] {
  def start(): F[Unit]
}

object Presentation {
  def make[F[_] : Temporal : NConsole]
  (slides: List[Slide[F]]): F[Presentation[F]] = Monad[F].pure(
    new Presentation[F] {
      override def start(): F[Unit] = for {
        _ <- NConsole[F].clear()
        _ <- executionLoop()
      } yield ()

      def executionLoop(): F[Int] =
        slides.head.show().start >> Monad[F].tailRecM(0) { currentSlideIndex =>
          for {
            input <- NConsole[F].read()
            _ <- slides(currentSlideIndex).userInput(input)
            slide <- input match {
              case Key(k) if k == SpecialKey.Left =>
                if (currentSlideIndex > 0) {
                  for {
                    _ <- NConsole[F].clear()
                    index = currentSlideIndex - 1
                    _ <- slides(index).show().start
                  } yield Either.left(index)
                } else {
                  Monad[F].pure(Either.left(currentSlideIndex))
                }
              case Key(k) if k == SpecialKey.Right =>
                if (currentSlideIndex < slides.length - 1) {
                  for {
                    _ <- NConsole[F].clear()
                    index = currentSlideIndex + 1
                    _ <- slides(index).show().start
                  } yield Either.left(index)
                } else {
                  Monad[F].pure(Either.left(currentSlideIndex))
                }
              case Key(k) if k == SpecialKey.Esc =>
                NConsole[F].clear() >>
                  Bye[F].show() >>
                  Temporal[F].sleep(500.milli) >>
                  NConsole[F].clear().as(Either.right(currentSlideIndex))
              case _ =>
                Monad[F].pure(Either.left(currentSlideIndex))
            }
          } yield slide
        }

    }
  )
}
