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
      .addSlide(Start())
      .addTransitions(right = ReplaceTransition(' '))
      .addSlide(Agenda())
      .addTransitions(right = MorphTransition())
      .addSlide(DistributedSystem())
      .addTransitions(right = MorphTransition())
      .addSlide(CascadingFailure1())
      .addTransitions(right = MorphTransition())
      .addSlide(CascadingFailure2())
      .addTransitions(right = MorphTransition())
      .addSlide(CascadingFailure3())
      .addTransitions(right = MorphTransition())
      .addSlide(Cause())
      .addTransitions(right = MorphTransition())
      .addSlide(TimeoutDoBetter1())
      .addTransitions(right = ReplaceTransition('?'))
      .addSlide(TimeoutDoBetter2())
      .addTransitions(right = MorphTransition())
      .addSlide(Timeout1())
      .addTransitions(right = MorphTransition())
      .addSlide(DistributedSystem2())
      .addTransitions(right = MorphTransition())
      .addSlide(Timeout2())
      .addTransitions(right = MorphTransition())
      .addSlide(CircuitBreakerDoBetter1())
      .addTransitions(right = ReplaceTransition('?'))
      .addSlide(CircuitBreakerDoBetter2())
      .addTransitions(right = MorphTransition())
      .addSlide(ThePattern1())
      .addTransitions(right = MorphTransition())
      .addSlide(ThePattern2())
      .addTransitions(right = MorphTransition())
      .addSlide(ThePattern3())
      .addTransitions(right = MorphTransition())
      .addSlide(ThePattern4())
      .addTransitions(right = MorphTransition())
      .addSlide(StateOfTheSystemAfterCircuitBreaker())
      .addTransitions(right = MorphTransition())
      .addSlide(InContrast1())
      .addTransitions(right = MorphTransition())
      .addSlide(InContrast2())
      .addTransitions(right = MorphTransition())
      .addSlide(InTheCode1())
      .addTransitions(right = MorphTransition())
      .addSlide(InTheCode2())
      .addTransitions(right = MorphTransition())
      .addSlide(DemoTime())
      .addTransitions(right = MorphTransition())
      .addSlide(ThePattern4())
      .addSlide(circuitBreakerSlide)
      .addTransitions(right = MorphTransition())
      .addSlide(Conclusion())
      .addTransitions(right = MorphTransition())
      .addSlide(References())
      .build()
    _ <- presentation.start()
  } yield ()

}
