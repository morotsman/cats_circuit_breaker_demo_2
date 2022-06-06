package com.github.morotsman
package presentation.slides.demo_slide

import cats._
import cats.effect._
import cats.implicits._
import presentation.demo.{SourceOfMayhem, Statistics}
import presentation.tools.{Character, Input}

import monocle.Lens
import monocle.macros.Lenses

@Lenses
final case class ControlPanelState[F[_]]
(
  previousInput: Option[Input],
)

object ControlPanelState {
  def make[F[_]](): ControlPanelState[F] = ControlPanelState[F](
    previousInput = None,
  )
}


trait ControlPanel[F[_]] {
  def getState(): F[ControlPanelState[F]]

  def userInput(input: Input): F[Unit]
}

object ControlPanel {
  def make[F[_] : Monad]
  (
    state: Ref[F, ControlPanelState[F]],
    sourceOfMayhem: SourceOfMayhem[F],
    demoProgramExecutor: DemoProgramExecutor[F],
    statistics: Statistics[F]
  ): ControlPanel[F] = new ControlPanel[F] {
    private val previousInput: Lens[ControlPanelState[F], Option[Input]] = ControlPanelState.previousInput[F]

    override def getState(): F[ControlPanelState[F]] = state.get

    override def userInput(input: Input): F[Unit] = input match {
      case Character(c) if c == 's' =>
        demoProgramExecutor.toggleStarted() >> statistics.currentInput(input)
      case Character(c) if c == 'f' =>
        sourceOfMayhem.toggleFailure() >> statistics.currentInput(input)
      case Character(c) if c == 'n' =>
        state.update(previousInput.replace(Option(input))) >> statistics.currentInput(input)
      case Character(c) if c == 'l' =>
        state.update(previousInput.replace(Option(input))) >> statistics.currentInput(input)
      case Character(c) if c == 't' =>
        state.update(previousInput.replace(Option(input))) >> statistics.currentInput(input)
      case Character(c) if c == 'a' =>
        state.update(previousInput.replace(Option(input))) >> statistics.currentInput(input)
      case Character(c) if c == 'r' =>
        state.update(previousInput.replace(Option(input))) >> statistics.currentInput(input)
      case Character(c) if c == 'm' =>
        state.update(previousInput.replace(Option(input))) >> statistics.currentInput(input)
      case Character(c) if c == '+' =>
        for {
          s <- state.get
          _ <- s.previousInput.traverse {
            case Character(c) if c == 'n' =>
              demoProgramExecutor.decreaseDelayBetweenCallsToSourceOfMayhem()
            case Character(c) if c == 'l' =>
              sourceOfMayhem.increaseSuccessLatency()
            case Character(c) if c == 't' =>
              sourceOfMayhem.increaseRequestTimeout()
            case Character(c) if c == 'a' =>
              demoProgramExecutor.increaseMaxFailures()
            case Character(c) if c == 'r' =>
              demoProgramExecutor.increaseResetTimeout()
            case Character(c) if c == 'm' =>
              demoProgramExecutor.increaseMaxResetTimeout()
            case _ =>
              Monad[F].unit
          }
        } yield ()
      case Character(c) if c == '-' =>
        for {
          s <- state.get
          _ <- s.previousInput.traverse {
            case Character(c) if c == 'n' =>
              demoProgramExecutor.increaseDelayBetweenCallsToSourceOfMayhem()
            case Character(c) if c == 'l' =>
              sourceOfMayhem.decreaseSuccessLatency()
            case Character(c) if c == 't' =>
              sourceOfMayhem.decreaseRequestTimeout()
            case Character(c) if c == 'a' =>
              demoProgramExecutor.decreaseMaxFailures()
            case Character(c) if c == 'r' =>
              demoProgramExecutor.decreaseResetTimeout()
            case Character(c) if c == 'm' =>
              demoProgramExecutor.decreaseMaxResetTimeout()
            case _ =>
              Monad[F].unit
          }
        } yield ()

      case _ =>
        Monad[F].unit
    }


  }
}
