package com.github.morotsman
package presentation.slides.timeout

import presentation.tools.{Input, NConsole, Slide}

import cats.effect.Sync

case class DistributedSystem2[F[_] : Sync : NConsole]() extends Slide[F] {
  private val distributedSystem =
    """
      |
      |
      |                                                                 ___      _        _   _ _ _                                 _      __
      |                                                                | _ )_  _| |_   __| |_(_) | |   __ __ _____  __ __ _____ _ _| |_   / _|_ _ ___ _ __
      |                                                                | _ \ || |  _| (_-<  _| | | |_  \ V  V / -_) \ V  V / -_) ' \  _| |  _| '_/ _ \ '  \
      |                                                                |___/\_,_|\__| /__/\__|_|_|_( )  \_/\_/\___|  \_/\_/\___|_||_\__| |_| |_| \___/_|_|_|
      |                                                                                            |/
      |
      |
      |
      |  ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___                           ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___                           ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___
      | |___|___|___|___|___|___|___|___|___|___|___|___|                         |___|___|___|___|___|___|___|___|___|___|___|___|                         |___|___|___|___|___|___|___|___|___|___|___|___|
      | | |                                           | |                         | |                                           | |                         | |                                           | |
      | | |                                           | |                         | |                                           | |                         | |                                           | |
      | | |                                           | |                         | |                                           | |                         | |                                           | |
      | |_|     ___              _            _       |_|        40 ms       __   |_|     ___              _          ___       |_|        30 ms       __   |_|     ___              _           ___      |_|
      | | |    / __| ___ _ ___ _(_)__ ___    /_\      | |  ___ ___ ___ ___ __\ \  | |    / __| ___ _ ___ _(_)__ ___  | _ )      | |  ___ ___ ___ ___ __\ \  | |    / __| ___ _ ___ _(_)__ ___   / __|     | |
      | | |    \__ \/ -_) '_\ V / / _/ -_)  / _ \     | | |___|___|___|___|___> > | |    \__ \/ -_) '_\ V / / _/ -_) | _ \      | | |___|___|___|___|___> > | |    \__ \/ -_) '_\ V / / _/ -_) | (__      | |
      | | |    |___/\___|_|  \_/|_\__\___| /_/ \_\    | |                    /_/  | |    |___/\___|_|  \_/|_\__\___| |___/      | |             | |    /_/  | |    |___/\___|_|  \_/|_\__\___|  \___|     | |
      | |_|                                           |_|                         |_|                                           |_|             | |         |_|                                           |_|
      | | |                                           | |                         | |                                           | |             | |         | |                                           | |
      | | |                                           | |                         | |                                           | |             | |         | |                                           | |
      | | |___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___| |                         | |___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___| |             | |         | |___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___| |
      | |_|___|___|___|___|___|___|___|___|___|___|___|_|                         |_|___|___|___|___|___|___|___|___|___|___|___|_|             | |         |_|___|___|___|___|___|___|___|___|___|___|___|_|
      |                                                                                                                                         | |
      |                                                                                                                                         | |
      |                                                                                                                                         | |
      |   ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___                           ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___             | |
      | |___|___|___|___|___|___|___|___|___|___|___|___|                         |___|___|___|___|___|___|___|___|___|___|___|___|             | |
      | | |                                           | |                         | |                                           | |             | |
      | | |                                           | |                         | |                                           | |             | |
      | | |                                           | |                         | |                                           | |             | |
      | |_|     ___              _           ___      |_|       35 ms        __   |_|     ___              _           ___      |_|    30 ms    | |
      | | |    / __| ___ _ ___ _(_)__ ___   |   \     | |  ___ ___ ___ ___ __\ \  | |    / __| ___ _ ___ _(_)__ ___   | __|     | |  ___ ___ ___| |
      | | |    \__ \/ -_) '_\ V / / _/ -_)  | |) |    | | |___|___|___|___|___> > | |    \__ \/ -_) '_\ V / / _/ -_)  | _|      | | |___|___|___|_|
      | | |    |___/\___|_|  \_/|_\__\___|  |___/     | |                    /_/  | |    |___/\___|_|  \_/|_\__\___|  |___|     | |
      | |_|                                           |_|                         |_|                                           |_|
      | | |                                           | |                         | |                                           | |
      | | |                                           | |                         | |                                           | |
      | | |___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___| |                         | |___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___| |
      | |_|___|___|___|___|___|___|___|___|___|___|___|_|                         |_|___|___|___|___|___|___|___|___|___|___|___|_|
      |
      |
      |
      |""".stripMargin



  override def show(): F[Unit] = NConsole[F].writeString(distributedSystem)

  override def userInput(input: Input): F[Unit] = Sync[F].unit
}