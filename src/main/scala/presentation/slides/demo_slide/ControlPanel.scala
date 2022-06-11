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
  input: Option[Input],
)

object ControlPanelState {
  def make[F[_]](): ControlPanelState[F] = ControlPanelState[F](
    input = None,
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
    demoProgramExecutor: DemoProgramExecutor[F]
  ): ControlPanel[F] = new ControlPanel[F] {
    private val input: Lens[ControlPanelState[F], Option[Input]] = ControlPanelState.input[F]

    override def getState(): F[ControlPanelState[F]] = state.get

    override def userInput(newInput: Input): F[Unit] = newInput match {
      case Character(c) if c == 's' =>
        demoProgramExecutor.toggleStarted()
      case Character(c) if c == 'f' =>
        sourceOfMayhem.toggleFailure()
      case Character(c) if c == 'n' =>
        state.update(input.replace(Option(newInput)))
      case Character(c) if c == 'l' =>
        state.update(input.replace(Option(newInput)))
      case Character(c) if c == 't' =>
        state.update(input.replace(Option(newInput)))
      case Character(c) if c == 'a' =>
        state.update(input.replace(Option(newInput)))
      case Character(c) if c == 'r' =>
        state.update(input.replace(Option(newInput)))
      case Character(c) if c == 'm' =>
        state.update(input.replace(Option(newInput)))
      case Character(c) if c == '+' =>
        for {
          s <- state.get
          _ <- s.input.traverse {
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
          _ <- s.input.traverse {
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
