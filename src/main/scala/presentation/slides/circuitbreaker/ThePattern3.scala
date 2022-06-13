package com.github.morotsman
package presentation.slides.circuitbreaker

import presentation.tools.{Input, NConsole, SimpleSlide, Slide}

import cats.effect.Sync

case class ThePattern3[F[_] : Sync : NConsole]() extends SimpleSlide[F] {
  val content =
    Sync[F].pure("""
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
      |                 .                                                                        .                                                                                .
      |                                                                                 fail a number of times
      |                                                                                increase failure counter
      |                 .                                                                        .                                                                                .
      |                 |                                                                        |                                                                                |
      |                | |-----------------------------call------------------------------------>| |                                                                               |
      |                | |                                                                      | |----------------------------------call--------------------------------------->| |
      |                | |                                                                      | |<--------------------------------timeout                                      | |
      |                | |<--------------------------timeout------------------------------------| | trip when threshold is exceeded                                               |
      |                 |                                                                        |                                                                                |
      |                 |                                                                        |                                                                                |
      |
      |""".stripMargin)

}
