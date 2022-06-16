package com.github.morotsman
package presentation.tools.transition

import presentation.tools.{NConsole, Slide}

import cats.effect.kernel.Temporal
import cats.implicits._

import scala.concurrent.duration.DurationInt
import scala.util.Random

object MorphTransition {
  def apply[F[_] : Temporal : NConsole](): Transition[F] = new Transition[F] {
    override def transition(from: Slide[F], to: Slide[F]): F[Unit] = {
      def morph(distortionRate: Double, from: String, to: String): F[Unit] = {
        if (distortionRate > 2) {
          NConsole[F].clear()
        } else {
          val morphedText = morphTheText(distortionRate, from, to)
          NConsole[F].clear() >>
            NConsole[F].writeStringCenterAligned(morphedText) >>
            Temporal[F].sleep(200.milli) >>
            morph(distortionRate * 2, morphedText, to)
        }
      }

      for {
        slide1 <- from.content
        slide2 <- to.content
        _ <- NConsole[F].writeStringCenterAligned(slide1) >> Temporal[F].sleep(2.seconds) >> morph(0.01, slide1, slide2)
      } yield ()
    }
  }

  private def morphTheText(distortionRate: Double, from: String, to: String): String = {
    val number = (from.length * distortionRate).toInt
    val numbers = Array.fill(number)(Random.nextInt(from.length))
    from.zipWithIndex.map { case (c, index) => if (numbers.contains(index) && c != '\n') {
      to.charAt(index)
    } else c
    }.mkString("")
  }

}
