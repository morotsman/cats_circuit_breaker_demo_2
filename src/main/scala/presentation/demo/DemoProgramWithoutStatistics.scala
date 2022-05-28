package com.github.morotsman
package presentation.demo

import cats.implicits._
import cats.MonadError
import io.chrisdavenport.circuit.CircuitBreaker

object DemoProgramWithoutStatistics {

  def make[F[_] : MonadError[*[_], Throwable]](
                                                sourceOfMayhem: SourceOfMayhem[F],
                                                circuitBreaker: CircuitBreaker[F],
                                              ): DemoProgram[F] = new DemoProgram[F] {
    override def run(): F[Unit] = for {
      _ <- circuitBreaker.protect {
        for {
          _ <- sourceOfMayhem.mightFail()
        } yield ()
      }.handleError(_ =>
        // Could return fallback value, or even call secondary service
        ()
      )
    } yield ()
  }

}
