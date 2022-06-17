package com.github.morotsman
package presentation.tools.transition

import presentation.tools.{NConsole, Slide}

import cats.Monad
import cats.effect.kernel.Temporal
import cats.implicits._

import scala.concurrent.duration.DurationInt
import scala.util.Random

case class IndexesToDrop(index: Int, numberOfRows: Int)

// TODO refactoring to remove duplicated code between transitions
object DropTransition {
  def apply[F[_] : Temporal : NConsole](): Transition[F] = new Transition[F] {
    override def transition(from: Slide[F], to: Slide[F]): F[Unit] = {

      def morph(distortionRate: Double, from: String, indexes: Set[IndexesToDrop]): F[Unit] = {
        if (distortionRate > 2) {
          NConsole[F].clear()
        } else {
          //val (newIndexes, text) = dropTheText(distortionRate, from, indexes)
          NConsole[F].clear() >>
            NConsole[F].writeString(from) >>
            Temporal[F].sleep(1000.milli) // >>
          //morph(distortionRate * 1.7, text, newIndexes)
        }
      }

      for {
        content <- from.content
        centerAligned <- NConsole[F].centerAlignText(content)
        _ <- NConsole[F].writeString(centerAligned) >> morph(0.01, centerAligned, Set.empty)
      } yield ()

    }
  }

  private def dropTheText(distortionRate: Double, from: String, indexesToDrop: Set[IndexesToDrop]): (Set[IndexesToDrop], String) = {
    val number = (from.length * distortionRate).toInt
    val newIndexesToDrop = Array.fill(number)(IndexesToDrop(Random.nextInt(from.length), 1)).toSet ++ indexesToDrop
    from.zipWithIndex.map { case (c, index) =>
      val maybeDrop = newIndexesToDrop.find(_.index == index)


      //if (numbers.contains(index) && c != '\n')
      ???

    }

    ???
  }

}
