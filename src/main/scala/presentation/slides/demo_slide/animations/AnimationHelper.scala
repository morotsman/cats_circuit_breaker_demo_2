package com.github.morotsman
package presentation.slides.demo_slide.animations

import presentation.demo.{MayhemState, StatisticsInfo}
import presentation.slides.demo_slide.CircuitBreakerConfiguration
import presentation.tools.{Character, Input}
import presentation.util.Colors._

object AnimationHelper {
  val mb = 1024*1024

  def showRuntimeInfo(
                       s: StatisticsInfo,
                       mayhemState: MayhemState,
                       circuitBreakerConfiguration: CircuitBreakerConfiguration
                     ) =
    raw"""
         |
         |
         |     ${showCircuitBreakerState(s, 40)} ${showSuccessLatency(mayhemState, 40)} ${showProgramCalled(s, 40)} ${showAverageProgramCallTime(s, 40)}
         |     ${showThreshold(circuitBreakerConfiguration, 40)} ${showRequestTimeout(mayhemState, 40)} ${showSourceOfMayhemCalled(s, 40)} ${showAverageSourceOfMayhemCallTime(s, 40)}
         |     ${showResetTimeout(circuitBreakerConfiguration, 40)} ${showPendingRequests(s, 40)}
         |     ${showMaxResetTimeout(circuitBreakerConfiguration, 40)} ${"Memory left: " + (Runtime.getRuntime.freeMemory() / mb) + " mb"}
         |
         |
         |""".stripMargin


  def startStop(isStarted: Boolean, width: Int): String =
    if (!isStarted) constantWidth("Start/Stop: s", width)
    else constantWidth(ANSI_GREEN + "Start/Stop: s" + ANSI_RESET, width + 9)

  def toggleFailure(mayhemState: MayhemState, width: Int): String =
    if (!mayhemState.isFailing) constantWidth("Toggle fail: f", width)
    else constantWidth(ANSI_GREEN + "Toggle fail: f" + ANSI_RESET, width + 9)

  def numberOfRequests(previousInput: Option[Input], width: Int): String = {
    previousInput.filter(_ == Character('n')).fold(constantWidth("Number of requests: n +/-", width)) { _ =>
      constantWidth(s"Number of requests: ${ANSI_GREEN + "n" + ANSI_RESET} +/-", width + 9)
    }
  }

  def timeout(previousInput: Option[Input], width: Int): String = {
    previousInput.filter(_ == Character('t')).fold(constantWidth("Timeout: t +/- ", width)) { _ =>
      constantWidth(s"Timeout: ${ANSI_GREEN + "t" + ANSI_RESET} +/-", width + 9)
    }
  }

  def threshold(previousInput: Option[Input], width: Int): String = {
    previousInput.filter(_ == Character('a')).fold(constantWidth("Failure threshold: a +/- ", width)) { _ =>
      constantWidth(s"Failure threshold: ${ANSI_GREEN + "a" + ANSI_RESET} +/-", width + 9)
    }
  }

  def resetTimeout(previousInput: Option[Input], width: Int): String = {
    previousInput.filter(_ == Character('r')).fold(constantWidth("Reset timeout: r +/-", width)) { _ =>
      constantWidth(s"Reset timeout: ${ANSI_GREEN + "r" + ANSI_RESET} +/-", width + 9)
    }
  }

  def maxResetTimeout(previousInput: Option[Input], width: Int): String = {
    previousInput.filter(_ == Character('m')).fold(constantWidth("Max reset timeout: m +/- ", width)) { _ =>
      constantWidth(s"Max reset timeout: ${ANSI_GREEN + "m" + ANSI_RESET} +/-", width + 9)
    }
  }

  def successLatency(previousInput: Option[Input], width: Int): String =
    previousInput.filter(_ == Character('l')).fold(constantWidth("Success latency: l +/-", width)) { _ =>
      constantWidth(s"Success latency: ${ANSI_GREEN + "l" + ANSI_RESET} +/-", width + 9)
    }

  def showSuccessLatency(s: MayhemState, width: Int): String =
    constantWidth(s"Success latency: ${s.successLatencyInMillis} ms", width)

  def showRequestTimeout(s: MayhemState, width: Int): String =
    constantWidth(s"Request timeout: ${s.requestTimeoutInMillis} ms", width)

  def showAverageProgramCallTime(s: StatisticsInfo, width: Int): String =
    if (s.programCompletedIn.nonEmpty) {
      constantWidth(s"Average time: ${s.programCompletedIn.sum / s.programCompletedIn.length} ms", width)
    } else {
      constantWidth(s"Average time: 0 ms", width)
    }

  def showAverageSourceOfMayhemCallTime(s: StatisticsInfo, width: Int): String =
    if (s.requestsCompletedIn.nonEmpty) {
      constantWidth(s"Average time: ${s.requestsCompletedIn.sum / s.requestsCompletedIn.length} ms", width)
    } else {
      constantWidth(s"Average time: 0 ms", width)
    }

  def showPendingRequests(s: StatisticsInfo, width: Int): String =
    constantWidth(s"Pending requests: ${s.pendingRequests}", width)

  def showSourceOfMayhemCalled(s: StatisticsInfo, width: Int): String =
    constantWidth(s"SourceOfMayhem called last second: ${s.sentSinceLastReport}", width)

  def showProgramCalled(s: StatisticsInfo, width: Int): String =
    constantWidth(s"Program called last second: ${s.programCalledSinceLastReport}", width)

  def showCircuitBreakerState(s: StatisticsInfo, width: Int): String =
    constantWidth(s"The Circuit breaker is ${s.circuitBreakerState.toString}", width)

  def showThreshold(s: CircuitBreakerConfiguration, width: Int): String =
    constantWidth(s"Threshold:  ${s.maxFailures} failure", width)

  def showResetTimeout(s: CircuitBreakerConfiguration, width: Int): String =
    constantWidth(s"Reset timeout:  ${s.resetTimeout.toSeconds} s", width)

  def showMaxResetTimeout(s: CircuitBreakerConfiguration, width: Int): String =
    constantWidth(s"Max reset timeout:  ${s.maxResetTimeout.toSeconds} s", width)

  private def constantWidth(s: String, width: Int): String =
    if (s.length > width) {
      s.take(width)
    } else if (s.length < width) {
      s + (" " * (width - s.length))
    } else s
}
