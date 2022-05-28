package com.github.morotsman
package presentation.slides

import presentation.tools.{Input, NConsole, Slide}

import cats.Monad
import cats.effect.Sync

case class Bye[F[_] : Monad : NConsole]() extends Slide[F] {
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
        |
        |
        |  ___          _
        | | _ )_  _ ___| |
        | | _ \ || / -_)_|
        | |___/\_, \___(_)
        |      |__/
        |
        |
        |
        |"""
    NConsole[F].writeString(text.stripMargin)
  }

  override def userInput(input: Input): F[Unit] = Monad[F].unit

}

