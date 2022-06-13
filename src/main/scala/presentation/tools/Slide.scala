package com.github.morotsman
package presentation.tools

trait Slide[F[_]] {
  def startShow(): F[Unit]

  def userInput(input: Input): F[Unit]
}
