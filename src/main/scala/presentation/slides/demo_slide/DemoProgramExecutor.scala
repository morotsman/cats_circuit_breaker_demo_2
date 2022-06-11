package com.github.morotsman
package presentation.slides.demo_slide

import cats.implicits._
import cats.effect.implicits._
import cats.{Applicative, Monad}
import cats.effect.{Ref, Temporal}
import presentation.demo.{CircuitBreakerState, DemoProgram, SourceOfMayhem, Statistics, StatisticsState}

import io.chrisdavenport.circuit.{Backoff, CircuitBreaker}
import monocle.Lens
import monocle.macros.Lenses

import scala.concurrent.duration.{DurationLong, FiniteDuration}

@Lenses
final case class CircuitBreakerConfiguration(
                                              maxFailures: Int,
                                              resetTimeout: FiniteDuration,
                                              maxResetTimeout: FiniteDuration
                                            )

object CircuitBreakerConfiguration {
  def make(): CircuitBreakerConfiguration = CircuitBreakerConfiguration(
    maxFailures = 4,
    resetTimeout = 3.seconds,
    maxResetTimeout = 30.seconds
  )
}

@Lenses
final case class DemoProgramExecutorState[F[_]]
(
  delayBetweenCallToSourceOfMayhemInNanos: Long,
  demoProgram: Option[DemoProgram[F]],
  circuitBreakerConfiguration: CircuitBreakerConfiguration,
  isStarted: Boolean,
  executorStartedListeners: List[ExecutorStartedListener[F]]
)

object DemoProgramExecutorState {
  private val oneSecondInNanos: Long = 1000 * 1000 * 1000

  def make[F[_]](): DemoProgramExecutorState[F] = DemoProgramExecutorState[F](
    delayBetweenCallToSourceOfMayhemInNanos = oneSecondInNanos,
    demoProgram = None,
    circuitBreakerConfiguration = CircuitBreakerConfiguration.make(),
    isStarted = false,
    executorStartedListeners = List.empty
  )
}

trait ExecutorStartedListener[F[_]] {
  def executorStarted(isStarted: Boolean): F[Unit]
}

trait DemoProgramExecutor[F[_]] {
  def execute(): F[Unit]

  def registerExecutorListener(listener: ExecutorStartedListener[F]): F[Unit]

  def increaseDelayBetweenCallsToSourceOfMayhem(): F[Unit]

  def decreaseDelayBetweenCallsToSourceOfMayhem(): F[Unit]

  def increaseResetTimeout(): F[Unit]

  def decreaseResetTimeout(): F[Unit]

  def increaseMaxFailures(): F[Unit]

  def decreaseMaxFailures(): F[Unit]

  def increaseMaxResetTimeout(): F[Unit]

  def decreaseMaxResetTimeout(): F[Unit]

  def toggleStarted(): F[Unit]

  def getState: F[DemoProgramExecutorState[F]]
}

object DemoProgramExecutor {
  def make[F[_] : Temporal]
  (
    state: Ref[F, DemoProgramExecutorState[F]],
    sourceOfMayhem: SourceOfMayhem[F],
    statistics: Statistics[F]
  ): F[DemoProgramExecutor[F]] = {
    val programCreator = createDemoProgram(sourceOfMayhem, statistics) _

    val demoProgram: Lens[DemoProgramExecutorState[F], Option[DemoProgram[F]]] = DemoProgramExecutorState.demoProgram[F]
    val executorStartedListeners = DemoProgramExecutorState.executorStartedListeners[F]
    val delayBetweenCallToSourceOfMayhemInNanos = DemoProgramExecutorState.delayBetweenCallToSourceOfMayhemInNanos[F]
    val circuitBreakerConfiguration = DemoProgramExecutorState.circuitBreakerConfiguration[F]
    val isStarted = DemoProgramExecutorState.isStarted[F]

    for {
      initialProgram <- programCreator(DemoProgramExecutorState.make())
      _ <- state.update(
        demoProgram.replace(Option(initialProgram))
      )
      result <- Monad[F].pure(new DemoProgramExecutor[F] {
        override def execute(): F[Unit] = for {
          s <- state.get
          _ <- if (s.isStarted) {
            Temporal[F].sleep(s.delayBetweenCallToSourceOfMayhemInNanos.nanos) >>
              s.demoProgram.traverse(_.run()).start
          } else Temporal[F].sleep(1.seconds)
          _ <- execute()
        } yield ()

        override def increaseDelayBetweenCallsToSourceOfMayhem(): F[Unit] = state.update(
          delayBetweenCallToSourceOfMayhemInNanos.modify(_ * 2)
        )

        override def decreaseDelayBetweenCallsToSourceOfMayhem(): F[Unit] = state.update(
          delayBetweenCallToSourceOfMayhemInNanos.modify(_ / 2)
        )

        override def increaseMaxFailures(): F[Unit] = updateProgramProperty {
          circuitBreakerConfiguration.andThen(CircuitBreakerConfiguration.maxFailures).modify(_ * 2)
        }

        override def decreaseMaxFailures(): F[Unit] = updateProgramProperty {
          circuitBreakerConfiguration.andThen(CircuitBreakerConfiguration.maxFailures).modify(_ / 2)
        }

        override def increaseResetTimeout(): F[Unit] = updateProgramProperty {
          circuitBreakerConfiguration.andThen(CircuitBreakerConfiguration.resetTimeout).modify(_ * 2)
        }

        override def decreaseResetTimeout(): F[Unit] = updateProgramProperty {
          circuitBreakerConfiguration.andThen(CircuitBreakerConfiguration.resetTimeout).modify(_ / 2)
        }

        override def increaseMaxResetTimeout(): F[Unit] = updateProgramProperty {
          circuitBreakerConfiguration.andThen(CircuitBreakerConfiguration.maxResetTimeout).modify(_ * 2)
        }

        override def decreaseMaxResetTimeout(): F[Unit] = updateProgramProperty {
          circuitBreakerConfiguration.andThen(CircuitBreakerConfiguration.maxResetTimeout).modify(_ / 2)
        }

        private def updateProgramProperty(modify: DemoProgramExecutorState[F] => DemoProgramExecutorState[F]): F[Unit] = for {
          s <- state.updateAndGet(modify)
          program <- programCreator(s)
          _ <- state.update(demoProgram.replace(Option(program)))
        } yield ()

        override def getState: F[DemoProgramExecutorState[F]] =
          state.get

        override def toggleStarted(): F[Unit] = for {
            updatedState <- state.updateAndGet(
              isStarted.modify(!_)
            )
            _ <- updatedState.executorStartedListeners.traverse(_.executorStarted(updatedState.isStarted))
          } yield ()

        override def registerExecutorListener(listener: ExecutorStartedListener[F]): F[Unit] =
          state.update(
            executorStartedListeners.modify(listener :: _)
          )
      })
    } yield result
  }


  private def createDemoProgram[F[_] : Temporal]
  (
    sourceOfMayhem: SourceOfMayhem[F],
    statistics: Statistics[F]
  )(state: DemoProgramExecutorState[F]): F[DemoProgram[F]] =
    CircuitBreaker.of[F](
      maxFailures = state.circuitBreakerConfiguration.maxFailures,
      resetTimeout = state.circuitBreakerConfiguration.resetTimeout,
      backoff = Backoff.exponential,
      maxResetTimeout = state.circuitBreakerConfiguration.maxResetTimeout,
      onOpen = statistics.circuitBreakerStateChange(CircuitBreakerState.OPEN),
      onClosed = statistics.circuitBreakerStateChange(CircuitBreakerState.CLOSED),
      onRejected = Applicative[F].unit,
      onHalfOpen = statistics.circuitBreakerStateChange(CircuitBreakerState.HALF_OPEN)
    ).map { circuitBreaker =>
      DemoProgram.make[F](
        sourceOfMayhem = sourceOfMayhem,
        circuitBreaker = circuitBreaker,
        statistics = statistics
      )
    }
}
