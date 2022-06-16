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

import com.github.morotsman.presentation.tools.transition.{MorphTransition, ReplaceTransition, TextTransition}

object Main extends IOApp.Simple {

  override def run(): IO[Unit] = for {
    circuitBreakerSlide <- CircuitBreakerSlide.make[IO]()
    presentation <- PresentationBuilder[IO]()
      .addSlide(Start().toSlide)
      .addTransitions(right = MorphTransition[IO]())
      .addSlide(Agenda().toSlide)
      .addTransitions(right = MorphTransition[IO]())
      .addSlide(DistributedSystem().toSlide)
      .addTransitions(right = MorphTransition[IO]())
      .addSlide(CascadingFailure1().toSlide)
      .addTransitions(right = MorphTransition[IO]())
      .addSlide(CascadingFailure2().toSlide)
      .addTransitions(right = MorphTransition[IO]())
      .addSlide(CascadingFailure3().toSlide)
      .addTransitions(right = MorphTransition[IO]())
      .addSlide(Cause().toSlide)
      .addTransitions(right = MorphTransition[IO]())
      .addSlide(TimeoutDoBetter1().toSlide)
      .addTransitions(right = MorphTransition[IO]())
      .addSlide(TimeoutDoBetter2().toSlide)
      .addTransitions(right = MorphTransition[IO]())
      .addSlide(Timeout1().toSlide)
      .addTransitions(right = MorphTransition[IO]())
      .addSlide(DistributedSystem2().toSlide)
      .addTransitions(right = MorphTransition[IO]())
      .addSlide(Timeout2().toSlide)
      .addTransitions(right = MorphTransition[IO]())
      .addSlide(CircuitBreakerDoBetter1().toSlide)
      .addTransitions(right = MorphTransition[IO]())
      .addSlide(CircuitBreakerDoBetter2().toSlide)
      .addTransitions(right = MorphTransition[IO]())
      .addSlide(ThePattern1().toSlide)
      .addTransitions(right = MorphTransition[IO]())
      .addSlide(ThePattern2().toSlide)
      .addTransitions(right = MorphTransition[IO]())
      .addSlide(ThePattern3().toSlide)
      .addTransitions(right = MorphTransition[IO]())
      .addSlide(ThePattern4().toSlide)
      .addTransitions(right = MorphTransition[IO]())
      .addSlide(StateOfTheSystemAfterCircuitBreaker().toSlide)
      .addTransitions(right = MorphTransition[IO]())
      .addSlide(InContrast1().toSlide)
      .addTransitions(right = MorphTransition[IO]())
      .addSlide(InContrast2().toSlide)
      .addTransitions(right = MorphTransition[IO]())
      .addSlide(InTheCode1().toSlide)
      .addTransitions(right = MorphTransition[IO]())
      .addSlide(InTheCode2().toSlide)
      .addTransitions(right = MorphTransition[IO]())
      .addSlide(DemoTime().toSlide)
      .addTransitions(right = MorphTransition[IO]())
      .addSlide(ThePattern4().toSlide)
      .addSlide(circuitBreakerSlide)
      .addTransitions(right = MorphTransition[IO]())
      .addSlide(Conclusion().toSlide)
      .addTransitions(right = MorphTransition[IO]())
      .addSlide(References().toSlide)
      .build()
    _ <- presentation.start()
  } yield ()

}
