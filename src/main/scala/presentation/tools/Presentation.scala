package com.github.morotsman
package presentation.tools

import cats._
import cats.effect._
import cats.effect.implicits._
import cats.implicits._
import presentation.tools.transition.Nothing

import scala.concurrent.duration.DurationInt

trait Presentation[F[_]] {
  def start(): F[Unit]
}

object Presentation {
  def make[F[_] : Temporal : NConsole]
  (sat: List[SlideAndTransition[F]]): F[Presentation[F]] = Monad[F].pure(
    new Presentation[F] {
      override def start(): F[Unit] = for {
        _ <- NConsole[F].clear()
        _ <- executionLoop()
      } yield ()

      def executionLoop(): F[(Int, Fiber[F, Throwable, Unit])] =
        sat.head.slide.startShow().start >>= (Monad[F].tailRecM(0, _) { case (currentSlideIndex, currentWork) =>
          for {
            input <- NConsole[F].read()
            result <- {
              val currentSat = sat(currentSlideIndex)
              input match {
                case Key(k) if k == SpecialKey.Right =>
                  if (currentSlideIndex < sat.length - 1) {
                    for {
                      _ <- currentSat.slide.stopShow()
                      _ <- currentWork.cancel
                      _ <- NConsole[F].clear()
                      index = currentSlideIndex + 1
                      nextSat = sat(index)
                      work <- {
                        (for {
                          _ <- currentSat.right.fold(Nothing().transition(currentSat.slide, nextSat.slide)) { right =>
                            right.transition(currentSat.slide, nextSat.slide)
                          }
                          _ <- NConsole[F].clear()
                          _ <- nextSat.left.fold(Nothing().transition(currentSat.slide, nextSat.slide)) { right =>
                            right.transition(currentSat.slide, nextSat.slide)
                          }
                          _ <- NConsole[F].clear()
                          _ <- nextSat.slide.startShow().start
                        } yield ()).start
                      }
                    } yield Either.left(index, work)
                  } else {
                    Monad[F].pure(Either.left(currentSlideIndex, currentWork))
                  }
                case Key(k) if k == SpecialKey.Left =>
                  if (currentSlideIndex > 0) {
                    for {
                      _ <- currentSat.slide.stopShow()
                      _ <- currentWork.cancel
                      _ <- NConsole[F].clear()
                      index = currentSlideIndex - 1
                      nextSat = sat(index)
                      work <- ( for {
                        _ <- currentSat.left.fold(Nothing().transition(currentSat.slide, nextSat.slide)) { right =>
                          right.transition(currentSat.slide, nextSat.slide)
                        }
                        _ <- NConsole[F].clear()
                        _ <- nextSat.right.fold(Nothing().transition(currentSat.slide, nextSat.slide)) { right =>
                          right.transition(currentSat.slide, nextSat.slide)
                        }
                        _ <- NConsole[F].clear()
                        _ <- nextSat.slide.startShow().start
                      } yield ()).start

                    } yield Either.left(index, work)
                  } else {
                    Monad[F].pure(Either.left(currentSlideIndex, currentWork))
                  }
                case Key(k) if k == SpecialKey.Esc =>
                  currentSat.slide.stopShow() >>
                    NConsole[F].clear() >>
                    Bye[F].startShow() >>
                    Temporal[F].sleep(500.milli) >>
                    NConsole[F].clear().as(Either.right(currentSlideIndex, currentWork))
                case _ =>
                  currentSat.slide.userInput(input) >>
                    Monad[F].pure(Either.left(currentSlideIndex, currentWork))
              }
            }
          } yield result
        })

    }
  )
}
