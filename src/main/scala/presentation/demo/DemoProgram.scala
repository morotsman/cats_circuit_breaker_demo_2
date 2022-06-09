package com.github.morotsman
package presentation.demo

import cats.implicits._
import cats.MonadError
import io.chrisdavenport.circuit.CircuitBreaker

trait DemoProgram[F[_]] {
  def run(): F[Unit]
}

object DemoProgram {

  def make[F[_] : MonadError[*[_], Throwable]](
                                                sourceOfMayhem: SourceOfMayhem[F],
                                                circuitBreaker: CircuitBreaker[F],
                                                statistics: Statistics[F]
                                              ): DemoProgram[F] = new DemoProgram[F] {
    override def run(): F[Unit] = for {
      _ <- statistics.programCalled()
      startTimeCallToProgram = System.currentTimeMillis()
      _ <- circuitBreaker.protect {
        for {
          _ <- statistics.requestSent()
          startTimeCallToSourceOfMayhem = System.currentTimeMillis()
          _ <- sourceOfMayhem.mightFail().attemptTap(_ =>
            statistics.requestCompleted() >>
              statistics.requestCompletedIn(System.currentTimeMillis() - startTimeCallToSourceOfMayhem)
          )
        } yield ()
      }.attemptTap(_ =>
        statistics.programCompletedIn(System.currentTimeMillis() - startTimeCallToProgram)
      ).handleError(_ => ())
    } yield ()
  }

}
