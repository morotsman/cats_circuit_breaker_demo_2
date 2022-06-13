package com.github.morotsman
package presentation.slides

import cats.effect.Sync
import presentation.tools.{NConsole, SimpleSlide}

case class Start[F[_] : Sync : NConsole]() extends SimpleSlide[F] {
  val content =
    Sync[F].pure("""
      |
      |
      |
      |
      |
      |
      |
      |
      |
      | _______ _________ _______  _______          __________________   ______   _______  _______  _______  _        _______  _______
      |(  ____ \\__   __/(  ____ )(  ____ \|\     /|\__   __/\__   __/  (  ___ \ (  ____ )(  ____ \(  ___  )| \    /\(  ____ \(  ____ )
      || (    \/   ) (   | (    )|| (    \/| )   ( |   ) (      ) (     | (   ) )| (    )|| (    \/| (   ) ||  \  / /| (    \/| (    )|
      || |         | |   | (____)|| |      | |   | |   | |      | |     | (__/ / | (____)|| (__    | (___) ||  (_/ / | (__    | (____)|
      || |         | |   |     __)| |      | |   | |   | |      | |     |  __ (  |     __)|  __)   |  ___  ||   _ (  |  __)   |     __)
      || |         | |   | (\ (   | |      | |   | |   | |      | |     | (  \ \ | (\ (   | (      | (   ) ||  ( \ \ | (      | (\ (
      || (____/\___) (___| ) \ \__| (____/\| (___) |___) (___   | |     | )___) )| ) \ \__| (____/\| )   ( ||  /  \ \| (____/\| ) \ \__
      |(_______/\_______/|/   \__/(_______/(_______)\_______/   )_(     |/ \___/ |/   \__/(_______/|/     \||_/    \/(_______/|/   \__/
      |
      |
      |""".stripMargin)
}

