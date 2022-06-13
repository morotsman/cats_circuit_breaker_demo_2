package com.github.morotsman
package presentation.slides.circuitbreaker

import presentation.tools.{NConsole, SimpleSlide}

import cats.effect.Sync

case class DemoTime[F[_] : Sync : NConsole]() extends SimpleSlide[F] {
  val content =
    """
      |
      |
      |  ___                  _   _           _
      | |   \ ___ _ __  ___  | |_(_)_ __  ___| |
      | | |) / -_) '  \/ _ \ |  _| | '  \/ -_)_|
      | |___/\___|_|_|_\___/  \__|_|_|_|_\___(_)
      |
      |
      |
      |
      |""".stripMargin

}
