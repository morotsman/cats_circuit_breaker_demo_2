package com.github.morotsman
package presentation.slides.circuitbreaker

import presentation.tools.{Input, NConsole, Slide}

import cats.effect.Sync

case class InTheCode2[F[_] : Sync : NConsole]() extends Slide[F] {
  val text =
    """
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
      |    CircuitBreaker.of[F](
      |      maxFailures = config.circuitBreakerConfiguration.maxFailures,
      |      resetTimeout = config.circuitBreakerConfiguration.resetTimeout,
      |      backoff = Backoff.exponential,
      |      maxResetTimeout = config.circuitBreakerConfiguration.maxResetTimeout,
      |      onOpen = statistics.circuitBreakerStateChange(CircuitBreakerState.OPEN),
      |      onClosed = statistics.circuitBreakerStateChange(CircuitBreakerState.CLOSED),
      |      onRejected = Applicative[F].unit,
      |      onHalfOpen = statistics.circuitBreakerStateChange(CircuitBreakerState.HALF_OPEN)
      |    ).map { circuitBreaker =>
      |      DemoProgramWithoutStatistics.make[F](
      |        sourceOfMayhem = sourceOfMayhem,
      |        circuitBreaker = circuitBreaker
      |      )
      |    }
      |
      |
      |""".stripMargin

  override def show(): F[Unit] = NConsole[F].writeString(text)

  override def userInput(input: Input): F[Unit] = Sync[F].unit
}
