package com.github.morotsman
package presentation.slides

import presentation.tools.{NConsole, SimpleSlide}

import cats.effect.Sync

case class Conclusion[F[_] : Sync : NConsole]() extends SimpleSlide[F] {
  val content =
    """
      |
      |
      |                                                              ___             _         _
      |                                                             / __|___ _ _  __| |_  _ __(_)___ _ _
      |                                                            | (__/ _ \ ' \/ _| | || (_-< / _ \ ' \
      |                                                             \___\___/_||_\__|_|\_,_/__/_\___/_||_|
      |
      |
      |
      |
      |  ___        _   _         _   _                                      _      _    _                __            _                             _  ____
      | | _ ) ___  | |_(_)_ _  __| | | |_ ___   _  _ ___ _  _ _ _   _ _  ___(_)__ _| |_ | |__  ___ _ _   / /_ _ _ _  __| |  _  _ ___ _  _ _ _ ___ ___| |/ _\ \
      | | _ \/ -_) | / / | ' \/ _` | |  _/ _ \ | || / _ \ || | '_| | ' \/ -_) / _` | ' \| '_ \/ _ \ '_| | / _` | ' \/ _` | | || / _ \ || | '_(_-</ -_) |  _|| |
      | |___/\___| |_\_\_|_||_\__,_|  \__\___/  \_, \___/\_,_|_|   |_||_\___|_\__, |_||_|_.__/\___/_|   | \__,_|_||_\__,_|  \_, \___/\_,_|_| /__/\___|_|_|  | |
      |                                         |__/                          |___/                      \_\                |__/                           /_/
      |
      |
      |
      |
      |
      |""".stripMargin

}

