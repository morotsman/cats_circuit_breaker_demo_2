package com.github.morotsman
package presentation

import cats.effect._
import presentation.slides.demo_slide.{CircuitBreakerSlide, ControlPanel, ControlPanelState, DemoProgramExecutor, DemoProgramExecutorState}
import presentation.slides.{Agenda, DistributedSystem, Start}
import presentation.tools.Presentation
import presentation.demo.{MayhemState, SourceOfMayhem, Statistics, StatisticsState}
import presentation.slides.demo_slide.animations.{Animator, AnimatorState}
import presentation.tools.NConsoleInstances.IONConsole
import presentation.slides.cascadingfailure.{CascadingFailure1, CascadingFailure2, CascadingFailure3}

object Main extends IOApp.Simple {

  override def run(): IO[Unit] = for {
      circuitBreakerSlide <- createCircuitBreakerSlide()
      presentation <- Presentation.make[IO](List(
        Start[IO],
        Agenda[IO],
        DistributedSystem[IO],
        CascadingFailure1[IO],
        CascadingFailure2[IO],
        CascadingFailure3[IO],
        circuitBreakerSlide
      ))
      _ <- presentation.start()
    } yield ()

  private def createCircuitBreakerSlide(): IO[CircuitBreakerSlide[IO]] = for {
    sourceOfMayhem <- Ref[IO].of(MayhemState.make()).map(SourceOfMayhem.make[IO])
    statistics <- Ref[IO].of(StatisticsState.make()).map(Statistics.make[IO])
    demoProgramExecutor <- Ref[IO].of(DemoProgramExecutorState.make[IO]()).flatMap(DemoProgramExecutor.make(
      _,
      sourceOfMayhem,
      statistics
    ))
    controlPanel <- Ref[IO].of(ControlPanelState.make[IO]()).map(ControlPanel.make[IO](
      _,
      sourceOfMayhem,
      demoProgramExecutor,
      statistics
    ))
    animator <- Ref[IO].of(AnimatorState.make()).map(
      Animator.make[IO](_, statistics, sourceOfMayhem, demoProgramExecutor)
    )
    circuitBreakerSlide = CircuitBreakerSlide[IO](
      sourceOfMayhem,
      statistics,
      demoProgramExecutor,
      controlPanel,
      animator
    )
  } yield circuitBreakerSlide
}
