package com.github.morotsman
package presentation.tools.transition

import presentation.tools.{NConsole, Slide}

import cats.effect.kernel.Temporal
import cats.implicits._

import scala.concurrent.duration.DurationInt
import scala.util.Random

object ReplaceTransition {
  def apply[F[_] : Temporal : NConsole](replace: Char): Transition[F] = new Transition[F] {
    override def transition(from: Slide[F], to: Slide[F]): F[Unit] = {
      def distort(distortionRate: Double, text: String): F[Unit] = {
        if (distortionRate > 2) {
          NConsole[F].clear()
        } else {
          val distortedText = distortTheText(distortionRate, text, replace)
          NConsole[F].clear() >>
            NConsole[F].writeStringCenterAligned(distortedText) >>
            Temporal[F].sleep(200.milli) >>
            distort(distortionRate * 2, distortedText)
        }
      }

      for {
        slide1 <- from.content
        _ <- NConsole[F].writeStringCenterAligned(slide1) >> Temporal[F].sleep(2.seconds) >> distort(0.01, slide1)
      } yield ()
    }
  }

  private def distortTheText(distortionRate: Double, text: String, replace: Char): String = {
    val number = (text.length * distortionRate).toInt
    val numbers = Array.fill(number)(Random.nextInt(text.length))
    text.zipWithIndex.map { case (c, index) => if (numbers.contains(index) && c != '\n') {
      replace
    } else c
    }.mkString("")
  }

}
