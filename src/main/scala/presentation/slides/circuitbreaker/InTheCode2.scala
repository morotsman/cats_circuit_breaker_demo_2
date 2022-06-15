package com.github.morotsman
package presentation.slides.circuitbreaker

object InTheCode2 {
  def apply(): String =
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

}
