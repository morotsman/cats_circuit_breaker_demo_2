package com.github.morotsman
package presentation.tools

import presentation.tools.SpecialKey.SpecialKey

import cats.effect.{IO, Sync}
import org.jline.terminal.TerminalBuilder
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

  def writeStringCenterAligned(s: String): F[Unit]

  def writeString(s: String): F[Unit]

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

        override def writeStringCenterAligned(s: String): F[Unit] = Sync[F].blocking {
          val width = terminal.getWidth
          val splitByNewLine = s.split("\n")
          val padFactor = splitByNewLine.map(line => (width - line.length) / 2).min
          val padding = Array.fill(padFactor)(" ").mkString("")
          val text = splitByNewLine.map { line =>
            if (padFactor >= 0) {
              padding + line
            } else ???
          }.mkString("\n")
          println(text)
        }


        override def writeString(s: String): F[Unit] = Sync[F].blocking {
          println(s)
        }

        override def clear(): F[Unit] = Sync[F].blocking {
          terminal.puts(Capability.clear_screen)
          terminal.flush()
        }
      }

    )
  }
}

object NConsoleInstances {
  implicit val IONConsole: NConsole[IO] = new NConsole[IO] {
    private val console = NConsole.make[IO]()

    override def read(): IO[Input] = console.flatMap(_.read())

    override def writeStringCenterAligned(s: String): IO[Unit] =
      console.flatMap(_.writeStringCenterAligned(s))

    override def clear(): IO[Unit] =
      console.flatMap(_.clear())

    override def writeString(s: String): IO[Unit] =
      console.flatMap(_.writeString(s))
  }
}
