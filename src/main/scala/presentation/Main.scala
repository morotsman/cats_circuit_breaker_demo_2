package com.github.morotsman
package presentation

import cats.effect._
import presentation.slides.demo_slide.CircuitBreakerSlide
import presentation.slides.{Agenda, Conclusion, DistributedSystem, References, Start}
import presentation.tools.Presentation
import presentation.tools.NConsoleInstances.IONConsole
import presentation.slides.cascadingfailure.{CascadingFailure1, CascadingFailure2, CascadingFailure3}

import com.github.morotsman.presentation.slides.cause.Cause
import com.github.morotsman.presentation.slides.circuitbreaker.{CircuitBreakerDoBetter1, CircuitBreakerDoBetter2, DemoTime, InContrast1, InContrast2, InTheCode1, InTheCode2, StateOfTheSystemAfterCircuitBreaker, ThePattern1, ThePattern2, ThePattern3, ThePattern4, ThePattern5}
import com.github.morotsman.presentation.slides.timeout.{DistributedSystem2, Timeout1, Timeout2, TimeoutDoBetter1, TimeoutDoBetter2}

object Main extends IOApp.Simple {

  override def run(): IO[Unit] = for {
      circuitBreakerSlide <- CircuitBreakerSlide.make[IO]()
      presentation <- Presentation.make[IO](List(
        Start[IO],
        Agenda[IO],
        DistributedSystem[IO],
        CascadingFailure1[IO],
        CascadingFailure2[IO],
        CascadingFailure3[IO],
        Cause[IO],
        TimeoutDoBetter1[IO],
        TimeoutDoBetter2[IO],
        Timeout1[IO],
        DistributedSystem2[IO],
        Timeout2[IO],
        CircuitBreakerDoBetter1[IO],
        CircuitBreakerDoBetter2[IO],
        ThePattern1[IO],
        ThePattern2[IO],
        ThePattern3[IO],
        ThePattern4[IO],
        StateOfTheSystemAfterCircuitBreaker[IO],
        InContrast1[IO],
        InContrast2[IO],
        DemoTime[IO],
        circuitBreakerSlide,
        InTheCode1[IO],
        InTheCode2[IO],
        Conclusion[IO],
        References[IO]
      ))
      _ <- presentation.start()
    } yield ()

}
