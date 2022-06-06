package com.github.morotsman
package presentation.slides.demo_slide.animations

import presentation.demo.{MayhemState, StatisticsInfo}
import presentation.tools.Input
import presentation.slides.demo_slide.CircuitBreakerConfiguration
import presentation.slides.demo_slide.animations.AnimationHelper._

object Static {

  val staticAnimation = List(
    (
      s: StatisticsInfo,
      p: Option[Input],
      mayhemState: MayhemState,
      circuitBreakerConfiguration: CircuitBreakerConfiguration,
      isStarted: Boolean
    ) =>
      showRuntimeInfo(s, mayhemState, circuitBreakerConfiguration) +
        raw"""
             |           __   Success                                                                                   __  call / raise circuit open
             |        _ / /__ ___ ___ ___ ___ _                                                                      _ / /__ ___ ___ ___ ___ _
             |       | < <___|___|___|___|___| |                                                                    | < <___|___|___|___|___| |
             |       | |\_\                  | |                                                                    | |\_\                  | |
             |       | |                     | |                                                                    | |                     | |
             |       | |                     | |                                                                    | |                     | |
             |  ___ _|_|___ ___ ___ ___ ___ _|_|___ ___           fail (threshold reached)               __    ___ _|_|___ ___ ___ ___ ___ _|_|___ ___
             | |___|___|___|___|___|___|___|___|___|___|      ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___\ \  |___|___|___|___|___|___|___|___|___|___|
             | |_|     ___ _    ___  ___ ___ ___     |_|      ___|___|___|___|___|___|___|___|___|___|___ > > |_|         ___  ___ ___ _  _         |_|
             | | |    / __| |  / _ \/ __| __|   \    | |     __                                          /_/  | |        / _ \| _ \ __| \| |        | |
             | | |   | (__| |_| (_) \__ \ _|| |) |   | |    / /___ ___ _                                      | |       | (_) |  _/ _|| .` |        | |
             | | |    \___|____\___/|___/___|___/    | |   < < ___|___| |                                     | |        \___/|_| |___|_|\_|        | |
             | |_|_ ___ ___ ___ ___ ___ ___ ___ ___ _|_|    \_\       | |                                     |_|_ ___ ___ ___ ___ ___ ___ ___ ___ _|_|
             | |___|___|___|___|___|___|___|___|___|___|              | |                                     |___|___|___|___|___|___|___|___|___|___|
             |     | |                     | |                        | |
             |     | | __                  | |                        | |                                         / \                 reset timeout
             |     | |/ /__ ___ ___ ___ ___| |                        | |                                        /| |\                    | |
             |     |_< <___|___|___|___|___|_|                        | |                                         | |                     | |
             |        \_\                                             | |                                         | |                     | |
             |           fail (under threshold)                       | |                                         |_|                     |_|
             |                                                        | |                                         | |                     | |
             |  ${startStop(isStarted, 34)}                    | |                                         | |                     | |
             |  ${toggleFailure(mayhemState, 40)}              | |                                         | |                    \|_|/
             |  ${numberOfRequests(p, 33)}                     | |                                        fail                     \ /
             |  ${successLatency(p, 31)}                       | |                            ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___
             |  ${timeout(p, 24)}                              | |                           |___|___|___|___|___|___|___|___|___|___|___|___|___|
             |  ${threshold(p, 44, s, true)}          | |___ ___ ___ ___ ___ ___    |_|   _  _   _   _    ___    ___  ___ ___ _  _    |_|
             |  ${resetTimeout(p, 47, s, true)}       |_ ___|___|___|___|___|___|   | |  | || | /_\ | |  | __|  / _ \| _ \ __| \| |   | |
             |  ${maxResetTimeout(p, 50, s, true)}           Success                | |  | __ |/ _ \| |__| _|  | (_) |  _/ _|| .` |   | |
             |                                                                                      | |  |_||_/_/ \_\____|_|    \___/|_| |___|_|\_|   | |
             |                                                                                      |_|_ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ ___ _|_|
             |                                                                                      |___|___|___|___|___|___|___|___|___|___|___|___|___|
             |
             |
             |""".stripMargin
  )

}
