package com.github.morotsman
package presentation.tools

import cats._
import cats.effect._
import cats.effect.implicits._
import cats.implicits._

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

      def executionLoop(): F[Unit] = {
        def loop(currentSlideIndex: Int = 0): F[Unit] = for {
          input <- NConsole[F].read()
          slide <- input match {
            case Key(k) if k == SpecialKey.Left =>
              if (currentSlideIndex > 0) {
                for {
                  _ <- slides(currentSlideIndex).userInput(input)
                  _ <- NConsole[F].clear()
                  index = currentSlideIndex - 1
                  _ <- slides(index).show().start
                } yield index
              } else {
                Monad[F].pure(currentSlideIndex)
              }
            case Key(k) if k == SpecialKey.Right =>
              if (currentSlideIndex < slides.length - 1) {
                for {
                  _ <- slides(currentSlideIndex).userInput(input)
                  _ <- NConsole[F].clear()
                  index = currentSlideIndex + 1
                  _x <- slides(index).show().start
                } yield index
              } else {
                Monad[F].pure(currentSlideIndex)
              }
            case _ =>
              slides(currentSlideIndex).userInput(input).as(currentSlideIndex)
          }
          _ <- loop(slide)
        } yield ()

        slides.head.show().start >> (loop(0))
      }
    }
  )
}
