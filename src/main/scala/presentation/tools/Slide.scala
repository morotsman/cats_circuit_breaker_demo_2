package com.github.morotsman
package presentation.tools

trait Slide[F[_]] {
  def content: F[String]

  def startShow(): F[Unit]

  def stopShow(): F[Unit]

  def userInput(input: Input): F[Unit]
}
