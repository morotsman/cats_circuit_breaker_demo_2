package com.github.morotsman
package presentation.slides.circuitbreaker

import presentation.tools.{Input, NConsole, Slide}

import cats.effect.Sync

case class ThePattern2[F[_] : Sync : NConsole]() extends Slide[F] {
  val text =
    """
      |
      |
      |
      |  ___ ___ ___ ___ ___ ___ ___ ___                          ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___                          ___ ___ ___ ___ ___ ___ ___ ___ ___ ___
      | |___|___|___|___|___|___|___|___|                        |___|___|___|___|___|___|___|___|___|___|___|___|___|___|___|___|___|                        |___|___|___|___|___|___|___|___|___|___|
      | | |                           | |                        | |                                                               | |                        | |                                   | |
      | |_|    ___ _ _         _      |_|                        |_|   ___ _             _ _     _                 _               |_|                        |_|  ___                _ _           |_|
      | | |   / __| (_)___ _ _| |_    | |                        | |  / __(_)_ _ __ _  _(_) |_  | |__ _ _ ___ __ _| |_____ _ _     | |                        | | / __|_  _ _ __ _ __| (_)___ _ _   | |
      | | |  | (__| | / -_) ' \  _|   | |                        | | | (__| | '_/ _| || | |  _| | '_ \ '_/ -_) _` | / / -_) '_|    | |                        | | \__ \ || | '_ \ '_ \ | / -_) '_|  | |
      | | |   \___|_|_\___|_||_\__|   | |                        | |  \___|_|_| \__|\_,_|_|\__| |_.__/_| \___\__,_|_\_\___|_|      | |                        | | |___/\_,_| .__/ .__/_|_\___|_|    | |
      | |_|                           |_|                        |_|                                                               |_|                        |_|          |_|  |_|                 |_|
      | | |___ ___ ___ ___ ___ ___ ___| |                        | |___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___| |                        | |___ ___ ___ ___ ___ ___ ___ ___ ___| |
      | |_|___|___|___|___|___|___|___|_|                        |_|___|___|___|___|___|___|___|___|___|___|___|___|___|___|___|___|_|                        |_|___|___|___|___|___|___|___|___|___|_|
      |                | |                                                                       |                                                                                |
      |                | |-----------------------------call------------------------------------>| |                                                                               |
      |                | |                                                                      | |----------------------------------call--------------------------------------->| |
      |                | |                                                                      | |<--------------------------------response-------------------------------------| |
      |                | |<--------------------------response-----------------------------------| |                                                                               |
      |                 |                                                                        |                                                                                |
      |                 |                                                                        |                                                                                |
      |                | |-----------------------------call------------------------------------>| |                                                                               |
      |                | |                                                                      | |----------------------------------call--------------------------------------->| |
      |                | |                                                                      | |<--------------------------------timeout                                      | |
      |                | |<--------------------------timeout------------------------------------| | increase failure counter                                                      |
      |                 |                                                                        |                                                                                |
      |                 |                                                                        |                                                                                |
      |""".stripMargin

  override def show(): F[Unit] = NConsole[F].writeString(text)

  override def userInput(input: Input): F[Unit] = Sync[F].unit
}
