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
        slides.head.startShow().start >> Monad[F].tailRecM(0) { currentSlideIndex =>
          for {
            input <- NConsole[F].read()
            result <- {
              val currentSlide = slides(currentSlideIndex)
              input match {
                case Key(k) if k == SpecialKey.Left =>
                  if (currentSlideIndex > 0) {
                    for {
                      _ <- currentSlide.stopShow()
                      _ <- NConsole[F].clear()
                      index = currentSlideIndex - 1
                      nextSlide = slides(index)
                      _ <- nextSlide.startShow().start
                    } yield Either.left(index)
                  } else {
                    Monad[F].pure(Either.left(currentSlideIndex))
                  }
                case Key(k) if k == SpecialKey.Right =>
                  if (currentSlideIndex < slides.length - 1) {
                    for {
                      _ <- currentSlide.stopShow()
                      _ <- NConsole[F].clear()
                      index = currentSlideIndex + 1
                      nextSlide = slides(index)
                      _ <- nextSlide.startShow().start
                    } yield Either.left(index)
                  } else {
                    Monad[F].pure(Either.left(currentSlideIndex))
                  }
                case Key(k) if k == SpecialKey.Esc =>
                  currentSlide.stopShow() >>
                    NConsole[F].clear() >>
                    Bye[F].startShow() >>
                    Temporal[F].sleep(500.milli) >>
                    NConsole[F].clear().as(Either.right(currentSlideIndex))
                case _ =>
                  currentSlide.userInput(input) >>
                    Monad[F].pure(Either.left(currentSlideIndex))
              }
            }
          } yield result
        }

    }
  )
}
