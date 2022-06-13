package com.github.morotsman
package presentation.slides.circuitbreaker

import presentation.tools.{Input, NConsole, SimpleSlide, Slide}

import cats.effect.Sync

case class ThePattern5[F[_] : Sync : NConsole]() extends SimpleSlide[F] {
  val content =
    """
      |
      |                                                       ___ _        _                       _    _
      |                                                      / __| |_ __ _| |_ ___   _ __  __ _ __| |_ (_)_ _  ___
      |                                                      \__ \  _/ _` |  _/ -_) | '  \/ _` / _| ' \| | ' \/ -_)
      |                                                      |___/\__\__,_|\__\___| |_|_|_\__,_\__|_||_|_|_||_\___|
      |
      |
      |
      |
      |           __   Success                                                                                   __  call / raise circuit open
      |        _ / /__ ___ ___ ___ ___ _                                                                      _ / /__ ___ ___ ___ ___ _
      |       | < <___|___|___|___|___| |                                                                    | < <___|___|___|___|___| |
      |       | |\_\                  | |                                                                    | |\_\                  | |
      |       | |                     | |                                                                    | |                     | |
      |       | |                     | |                                                                    | |                     | |
      |  ___ _|_|___ ___ ___ ___ ___ _|_|___ ___           fail (threshold reached)               __    ___ _|_|___ ___ ___ ___ ___ _|_|___ ___
      | |___|___|___|___|___|___|___|___|___|___|      ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___\ \  |___|___|___|___|___|___|___|___|___|___|
      | |_|     ___ _    ___  ___ ___ ___     |_|      ___|___|___|___|___|___|___|___|___|___|___ > > |_|         ___  ___ ___ _  _         |_|
      | | |    / __| |  / _ \/ __| __|   \    | |     __                                          /_/  | |        / _ \| _ \ __| \| |        | |
      | | |   | (__| |_| (_) \__ \ _|| |) |   | |    / /___ ___ _                                      | |       | (_) |  _/ _|| .` |        | |
      | | |    \___|____\___/|___/___|___/    | |   < < ___|___| |                                     | |        \___/|_| |___|_|\_|        | |
      | |_|_ ___ ___ ___ ___ ___ ___ ___ ___ _|_|    \_\       | |                                     |_|_ ___ ___ ___ ___ ___ ___ ___ ___ _|_|
      | |___|___|___|___|___|___|___|___|___|___|              | |                                     |___|___|___|___|___|___|___|___|___|___|
      |     | |                     | |                        | |
      |     | | __                  | |                        | |                                         / \                 reset timeout
      |     | |/ /__ ___ ___ ___ ___| |                        | |                                        /| |\                    | |
      |     |_< <___|___|___|___|___|_|                        | |                                         | |                     | |
      |        \_\                                             | |                                         | |                     | |
      |           fail (under threshold)                       | |                                         |_|                     |_|
      |                                                        | |                                         | |                     | |
      |                                                        | |                                         | |                     | |
      |                                                        | |                                         | |                    \|_|/
      |                                                        | |                                        fail                     \ /
      |                                                        | |                            ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___
      |                                                        | |                           |___|___|___|___|___|___|___|___|___|___|___|___|___|
      |                                                        | |___ ___ ___ ___ ___ ___    |_|   _  _   _   _    ___    ___  ___ ___ _  _    |_|
      |                                                        |_ ___|___|___|___|___|___|   | |  | || | /_\ | |  | __|  / _ \| _ \ __| \| |   | |
      |                                                               Success                | |  | __ |/ _ \| |__| _|  | (_) |  _/ _|| .` |   | |
      |                                                                                      | |  |_||_/_/ \_\____|_|    \___/|_| |___|_|\_|   | |
      |                                                                                      |_|_ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ _|_|
      |                                                                                      |___|___|___|___|___|___|___|___|___|___|___|___|___|
      |""".stripMargin

}
