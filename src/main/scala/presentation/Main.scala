package com.github.morotsman
package presentation

import cats.implicits._
import cats.effect._
import presentation.slides.demo_slide.CircuitBreakerSlide
import presentation.slides.{Agenda, Conclusion, DistributedSystem, References, Start}
import presentation.tools.PresentationBuilder
import presentation.tools.NConsoleInstances.IONConsole
import presentation.slides.cascadingfailure.{CascadingFailure1, CascadingFailure2, CascadingFailure3}
import presentation.slides.cause.Cause
import presentation.slides.circuitbreaker.{CircuitBreakerDoBetter1, CircuitBreakerDoBetter2, DemoTime, InContrast1, InContrast2, InTheCode1, InTheCode2, StateOfTheSystemAfterCircuitBreaker, ThePattern1, ThePattern2, ThePattern3, ThePattern4}
import presentation.slides.timeout.{DistributedSystem2, Timeout1, Timeout2, TimeoutDoBetter1, TimeoutDoBetter2}
import presentation.tools.SimpleSlide._

import com.github.morotsman.presentation.tools.transition.{ReplaceTransition, TextTransistion}

object Main extends IOApp.Simple {

  override def run(): IO[Unit] = for {
    circuitBreakerSlide <- CircuitBreakerSlide.make[IO]()
    presentation <- PresentationBuilder[IO]()
      .addSlide(Start().toSlide)
      .addTransitions(right = ReplaceTransition[IO](' '))
      .addSlide(Agenda().toSlide)
      .addSlide(DistributedSystem().toSlide)
      .addSlide(CascadingFailure1().toSlide)
      .addSlide(CascadingFailure2().toSlide)
      .addSlide(CascadingFailure3().toSlide)
      .addSlide(Cause().toSlide)
      .addSlide(TimeoutDoBetter1().toSlide)
      .addSlide(TimeoutDoBetter2().toSlide)
      .addSlide(Timeout1().toSlide)
      .addSlide(DistributedSystem2().toSlide)
      .addSlide(Timeout2().toSlide)
      .addSlide(CircuitBreakerDoBetter1().toSlide)
      .addSlide(CircuitBreakerDoBetter2().toSlide)
      .addSlide(ThePattern1().toSlide)
      .addSlide(ThePattern2().toSlide)
      .addSlide(ThePattern3().toSlide)
      .addSlide(ThePattern4().toSlide)
      .addSlide(StateOfTheSystemAfterCircuitBreaker().toSlide)
      .addSlide(InContrast1().toSlide)
      .addSlide(InContrast2().toSlide)
      .addSlide(InTheCode1().toSlide)
      .addSlide(InTheCode2().toSlide)
      .addSlide(DemoTime().toSlide)
      .addSlide(ThePattern4().toSlide)
      .addSlide(circuitBreakerSlide)
      .addSlide(Conclusion().toSlide)
      .addSlide(References().toSlide)
      .build()
    _ <- presentation.start()
  } yield ()

}
