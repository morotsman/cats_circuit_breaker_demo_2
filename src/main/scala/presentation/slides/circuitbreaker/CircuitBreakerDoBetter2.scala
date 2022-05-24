package com.github.morotsman
package presentation.slides.circuitbreaker

import presentation.tools.{Input, NConsole, Slide}

import cats.effect.Sync

case class CircuitBreakerDoBetter2[F[_] : Sync : NConsole]() extends Slide[F] {
  val text =
    """
      |
      |
      |                                                   ___                             _       _         _   _          ___
      |                                                  / __|__ _ _ _   __ __ _____   __| |___  | |__  ___| |_| |_ ___ _ |__ \
      |                                                 | (__/ _` | ' \  \ V  V / -_) / _` / _ \ | '_ \/ -_)  _|  _/ -_) '_|/_/
      |                                                  \___\__,_|_||_|  \_/\_/\___| \__,_\___/ |_.__/\___|\__|\__\___|_| (_)
      |
      |
      |
      |                          __   __          _           _     _               _         _                _             _ _     _                 _              _
      |                          \ \ / /__ ___   | |__ _  _  (_)_ _| |_ _ _ ___  __| |_  _ __(_)_ _  __ _   __(_)_ _ __ _  _(_) |_  | |__ _ _ ___ __ _| |_____ _ _ __| |
      |                           \ V / -_|_-<_  | '_ \ || | | | ' \  _| '_/ _ \/ _` | || / _| | ' \/ _` | / _| | '_/ _| || | |  _| | '_ \ '_/ -_) _` | / / -_) '_(_-<_|
      |                            |_|\___/__( ) |_.__/\_, | |_|_||_\__|_| \___/\__,_|\_,_\__|_|_||_\__, | \__|_|_| \__|\_,_|_|\__| |_.__/_| \___\__,_|_\_\___|_| /__(_)
      |                                      |/        |__/                                         |___/
      |
      |
      |
      |""".stripMargin

  override def show(): F[Unit] = NConsole[F].writeString(text)

  override def userInput(input: Input): F[Unit] = Sync[F].unit
}
