package com.github.morotsman
package presentation.slides

import presentation.tools.{Input, NConsole, Slide}

import cats.implicits._
import cats.Monad
import cats.effect.kernel.Temporal

import scala.concurrent.duration.DurationInt
import scala.util.Random

case class Bye[F[_] : NConsole : Temporal]() extends Slide[F] {
  override def show(): F[Unit] = {
    val text =
      """
        |
        |
        |
        |
        |
        |
        |
        |                                                       /$$$$$$   /$$$$$$   /$$$$$$  /$$$$$$$  /$$$$$$$  /$$     /$$ /$$$$$$$$ /$$
        |                                                      /$$__  $$ /$$__  $$ /$$__  $$| $$__  $$| $$__  $$|  $$   /$$/| $$_____/| $$
        |                                                     | $$  \__/| $$  \ $$| $$  \ $$| $$  \ $$| $$  \ $$ \  $$ /$$/ | $$      | $$
        |                                                     | $$ /$$$$| $$  | $$| $$  | $$| $$  | $$| $$$$$$$   \  $$$$/  | $$$$$   | $$
        |                                                     | $$|_  $$| $$  | $$| $$  | $$| $$  | $$| $$__  $$   \  $$/   | $$__/   |__/
        |                                                     | $$  \ $$| $$  | $$| $$  | $$| $$  | $$| $$  \ $$    | $$    | $$
        |                                                     |  $$$$$$/|  $$$$$$/|  $$$$$$/| $$$$$$$/| $$$$$$$/    | $$    | $$$$$$$$ /$$
        |                                                      \______/  \______/  \______/ |_______/ |_______/     |__/    |________/|__/
        |
        |
        |
        |
        |
        |
        |
        |
        |
        |
        |
        |""".stripMargin

    def distort(distortionRate: Double, text: String): F[Unit] = {
      if (distortionRate > 2) {
        NConsole[F].clear()
      } else {
        val distortedText = distortTheText(distortionRate, text)
        NConsole[F].clear() >>
          NConsole[F].writeString(distortedText) >>
          Temporal[F].sleep(200.milli) >>
          distort(distortionRate * 2, distortedText)
      }
    }

    NConsole[F].writeString(text) >> Temporal[F].sleep(4.seconds) >> distort(0.01, text)
  }

  private def distortTheText(distortionRate: Double, text: String): String = {
    val number = (text.size * distortionRate).toInt
    val numbers = Array.fill(number)(Random.nextInt(text.length))
    text.zipWithIndex.map { case (c, index) => if (numbers.contains(index) && c != '\n') {
      ' '
    } else c
    }.mkString("")
  }


  override def userInput(input: Input): F[Unit] = Monad[F].unit

}

