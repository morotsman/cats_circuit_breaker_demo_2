package com.github.morotsman
package presentation.slides.circuitbreaker

import presentation.tools.{Input, NConsole, SimpleSlide, Slide}

import cats.effect.Sync

case class InTheCode1[F[_] : Sync : NConsole]() extends SimpleSlide[F] {
  val content =
    Sync[F].pure("""
      |
      |
      |  ___        _   _                     _
      | |_ _|_ _   | |_| |_  ___   __ ___  __| |___
      |  | || ' \  |  _| ' \/ -_) / _/ _ \/ _` / -_)
      | |___|_||_|  \__|_||_\___| \__\___/\__,_\___|
      |
      |
      |
      |
      |import cats.implicits._
      |import cats.MonadError
      |import io.chrisdavenport.circuit.CircuitBreaker
      |
      |object DemoProgramWithoutStatistics {
      |
      |  def make[F[_] : MonadError[*[_], Throwable]](
      |                                                sourceOfMayhem: SourceOfMayhem[F],
      |                                                circuitBreaker: CircuitBreaker[F],
      |                                              ): DemoProgram[F] = new DemoProgram[F] {
      |    override def run(): F[Unit] = for {
      |      _ <- circuitBreaker.protect {
      |        for {
      |          _ <- sourceOfMayhem.mightFail()
      |          // you might want to check what kind of error that was thrown,
      |          // in case that not all errors should cause the circuit breaker to trip
      |          // timeout (or other transient errors) vs business errors
      |        } yield ()
      |      }.handleError(_ =>
      |        // Could return fallback value, or even call secondary service
      |        ()
      |      )
      |    } yield ()
      |  }
      |
      |}
      |
      |
      |""".stripMargin)

}
