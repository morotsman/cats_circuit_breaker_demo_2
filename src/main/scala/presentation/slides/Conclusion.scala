package com.github.morotsman
package presentation.slides

import presentation.tools.{Input, NConsole, Slide}

import cats.effect.Sync

case class Conclusion[F[_] : Sync : NConsole]() extends Slide[F] {
  override def show(): F[Unit] = {
    val text =
      """
        |
        |
        |   ___             _         _
        |  / __|___ _ _  __| |_  _ __(_)___ _ _
        | | (__/ _ \ ' \/ _| | || (_-< / _ \ ' \
        |  \___\___/_||_\__|_|\_,_/__/_\___/_||_|
        |
        |
        |
        |"""
    NConsole[F].writeString(text.stripMargin)
  }

  override def userInput(input: Input): F[Unit] = Sync[F].unit

}

