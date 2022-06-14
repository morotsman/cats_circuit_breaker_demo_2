package com.github.morotsman
package presentation

import cats.implicits._
import cats.effect._
import presentation.slides.demo_slide.CircuitBreakerSlide
import presentation.slides.{Agenda, Conclusion, DistributedSystem, References, Start}
import presentation.tools.{Presentation, SimpleSlide}
import presentation.tools.NConsoleInstances.IONConsole
import presentation.slides.cascadingfailure.{CascadingFailure1, CascadingFailure2, CascadingFailure3}
import presentation.slides.cause.Cause
import presentation.slides.circuitbreaker.{CircuitBreakerDoBetter1, CircuitBreakerDoBetter2, DemoTime, InContrast1, InContrast2, InTheCode1, InTheCode2, StateOfTheSystemAfterCircuitBreaker, ThePattern1, ThePattern2, ThePattern3, ThePattern4}
import presentation.slides.timeout.{DistributedSystem2, Timeout1, Timeout2, TimeoutDoBetter1, TimeoutDoBetter2}

object Main extends IOApp.Simple {

  override def run(): IO[Unit] = for {
    circuitBreakerSlide <- CircuitBreakerSlide.make[IO]()
    presentation <- Presentation.make[IO](List(
      SimpleSlide[IO](Start.content),
      SimpleSlide[IO](Agenda.content),
      SimpleSlide[IO](DistributedSystem.content),
      SimpleSlide[IO](CascadingFailure1.content),
      SimpleSlide[IO](CascadingFailure2.content),
      SimpleSlide[IO](CascadingFailure3.content),
      SimpleSlide[IO](Cause.content),
      SimpleSlide[IO](TimeoutDoBetter1.content),
      SimpleSlide[IO](TimeoutDoBetter2.content),
      SimpleSlide[IO](Timeout1.content),
      SimpleSlide[IO](DistributedSystem2.content),
      SimpleSlide[IO](Timeout2.content),
      SimpleSlide[IO](CircuitBreakerDoBetter1.content),
      SimpleSlide[IO](CircuitBreakerDoBetter2.content),
      SimpleSlide[IO](ThePattern1.content),
      SimpleSlide[IO](ThePattern2.content),
      SimpleSlide[IO](ThePattern3.content),
      SimpleSlide[IO](ThePattern4.content),
      SimpleSlide[IO](StateOfTheSystemAfterCircuitBreaker.content),
      SimpleSlide[IO](InContrast1.content),
      SimpleSlide[IO](InContrast2.content),
      SimpleSlide[IO](InTheCode1.content),
      SimpleSlide[IO](InTheCode2.content),
      SimpleSlide[IO](DemoTime.content),
      SimpleSlide[IO](ThePattern4.content),
      circuitBreakerSlide,
      SimpleSlide[IO](Conclusion.content),
      SimpleSlide[IO](References.content)
    ))
    _ <- presentation.start()
  } yield ()

}
