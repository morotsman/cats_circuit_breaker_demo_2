package com.github.morotsman
package presentation.tools

import presentation.tools.SpecialKey.SpecialKey

import cats.effect.{IO, Sync}
import org.jline.terminal.{Terminal, TerminalBuilder}
import org.jline.utils.InfoCmp.Capability

sealed trait Input

final case class Key(k: SpecialKey) extends Input

final case class Character(c: Char) extends Input


object SpecialKey extends Enumeration {
  type SpecialKey = Value
  val Up, Down, Left, Right, Esc, Unknown = Value
}

trait NConsole[F[_]] {
  def read(): F[Input]

  def writeString(s: String, centerText: Boolean = true): F[Unit]

  def clear(): F[Unit]
}

object NConsole {
  @inline def apply[F[_]](implicit instance: NConsole[F]): NConsole[F] = instance

  private val terminal = TerminalBuilder.terminal()
  terminal.enterRawMode()
  private val reader = terminal.reader()

  def make[F[_] : Sync](): F[NConsole[F]] = {
    Sync[F].delay(
      new NConsole[F] {
        override def read(): F[Input] = Sync[F].blocking {
          var input = reader.read().toChar
          if (input == 27) {
            input = reader.read().toChar
            if (input == '[') {
              input = reader.read().toChar
              input match {
                case 'A' => Key(SpecialKey.Up)
                case 'D' => Key(SpecialKey.Left)
                case 'C' => Key(SpecialKey.Right)
                case 'B' => Key(SpecialKey.Down)
                case _ => Key(SpecialKey.Unknown)
              }
            } else {
              Key(SpecialKey.Esc)
            }
          } else {
            Character(input)
          }
        }

        override def writeString(s: String, centerText: Boolean = true): F[Unit] = Sync[F].blocking {
          val text = if (centerText) {
            val width = terminal.getWidth
            val height = terminal.getHeight
            val splitByNewLine = s.split("\n")
            val padFactor = splitByNewLine.map(line => (width - line.length) / 2).min
            splitByNewLine.map { line =>
              if (padFactor >= 0) {
                val padding = Array.fill(padFactor)(" ").mkString("")
                padding + line
              } else ???
            }.mkString("\n")
          } else {
            s
          }

          println(text)
        }

        override def clear(): F[Unit] = Sync[F].blocking {
          terminal.puts(Capability.clear_screen)
          terminal.flush()
        }
      })
  }
}

object NConsoleInstances {
  implicit val IONConsole: NConsole[IO] = new NConsole[IO] {
    private val console = NConsole.make[IO]()

    override def read(): IO[Input] = console.flatMap(_.read())

    override def writeString(s: String, centerText: Boolean = true): IO[Unit] =
      console.flatMap(_.writeString(s, centerText))

    override def clear(): IO[Unit] =
      console.flatMap(_.clear())
  }
}
