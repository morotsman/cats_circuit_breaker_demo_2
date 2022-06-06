package com.github.morotsman
package presentation.tools

import cats._
import cats.effect._
import cats.effect.implicits._
import cats.implicits._
import presentation.slides.Bye

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
                } yield Option(index)
              } else {
                Monad[F].pure(Option(currentSlideIndex))
              }
            case Key(k) if k == SpecialKey.Right =>
              if (currentSlideIndex < slides.length - 1) {
                for {
                  _ <- slides(currentSlideIndex).userInput(input)
                  _ <- NConsole[F].clear()
                  index = currentSlideIndex + 1
                  _x <- slides(index).show().start
                } yield Option(index)
              } else {
                Monad[F].pure(Option(currentSlideIndex))
              }
            case Key(k) if k == SpecialKey.Esc =>
              Monad[F].pure(None)
            case _ =>
              slides(currentSlideIndex).userInput(input).as(Option(currentSlideIndex))
          }
          _ <- slide.fold(
            NConsole[F].clear() >>
              Bye[F].show() >>
              Temporal[F].sleep(500.milli) >>
              NConsole[F].clear() >>
              Monad[F].unit
          )(loop)
        } yield ()

        slides.head.show().start >> loop()
      }
    }
  )
}
