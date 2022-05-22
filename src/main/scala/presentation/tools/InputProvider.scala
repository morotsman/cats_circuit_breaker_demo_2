package com.github.morotsman
package presentation.tools

import cats.FlatMap
import cats.implicits._

trait InputProvider[F[_]] {
  def provide(): F[Unit]
}

trait InputConsumer[F[_]] {
  def consume(input: Input): F[Unit]
}

object InputProvider {
  def make[F[_] : FlatMap](
                  inputConsumer: InputConsumer[F],
                  console: NConsole[F]
                ): InputProvider[F] = new InputProvider[F] {
    override def provide(): F[Unit] = for {
      input <- console.read()
      _ <- inputConsumer.consume(input)
      _ <- provide()
    } yield ()
  }
}
