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
        |"""
    NConsole[F].writeString(text.stripMargin)
  }

  override def userInput(input: Input): F[Unit] = Sync[F].unit

}

