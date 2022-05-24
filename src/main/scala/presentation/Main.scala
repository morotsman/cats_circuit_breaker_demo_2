package com.github.morotsman
package presentation

import cats.effect._
import presentation.slides.demo_slide.CircuitBreakerSlide
import presentation.slides.{Agenda, DistributedSystem, Start}
import presentation.tools.Presentation
import presentation.tools.NConsoleInstances.IONConsole
import presentation.slides.cascadingfailure.{CascadingFailure1, CascadingFailure2, CascadingFailure3}

import com.github.morotsman.presentation.slides.cause.Cause
import com.github.morotsman.presentation.slides.timeout.{Timeout1, Timeout2}

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
        Timeout1[IO],
        DistributedSystem[IO],
        Timeout2[IO],
        circuitBreakerSlide
      ))
      _ <- presentation.start()
    } yield ()

}
