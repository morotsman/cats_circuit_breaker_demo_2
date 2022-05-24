package com.github.morotsman
package presentation.slides.demo_slide.animations

import presentation.slides.demo_slide.animations.Static.staticAnimation

import com.github.morotsman.presentation.slides.demo_slide.animations.ClosedFailure.ClosedFailureAnimation
import com.github.morotsman.presentation.slides.demo_slide.animations.ClosedSuccess.ClosedSuccessAnimation

object AnimationState extends Enumeration {
  type AnimationState = Value
  val TRANSFER_HALF_OPEN_TO_OPEN, NOT_STARTED, UNKNOWN, CLOSED_SUCCEED, CLOSED_FAILING, TRANSFER_CLOSED_TO_OPEN, OPEN, TRANSFER_OPEN_TO_HALF_OPEN, HALF_OPEN, TRANSFER_HALF_OPEN_TO_CLOSED = Value

  val AnimationMapper = Map(
    NOT_STARTED -> staticAnimation,
    UNKNOWN -> staticAnimation,
    CLOSED_SUCCEED -> ClosedSuccessAnimation,
    CLOSED_FAILING -> ClosedFailureAnimation,
    TRANSFER_CLOSED_TO_OPEN -> TransferClosedToOpen.animation,
    OPEN -> Open.animation,
    TRANSFER_OPEN_TO_HALF_OPEN -> TransferOpenToHalfOpen.animation,
    HALF_OPEN -> Open.animation,
    TRANSFER_HALF_OPEN_TO_OPEN -> TransferHalfOpenToOpen.animation,
    TRANSFER_HALF_OPEN_TO_CLOSED -> TransferHalfOpenToClosed.animation
  )
}
