package com.github.morotsman
package presentation.tools

import cats.effect.Temporal
import presentation.tools.transition.Transition

final case class SlideAndTransition[F[_]](
                                           left: Option[Transition[F]],
                                           slide: Slide[F],
                                           right: Option[Transition[F]]
                                         )


sealed trait BuildState

sealed trait NotStarted extends BuildState

sealed trait Started extends BuildState

sealed trait InProgress extends Started

case class PresentationBuilder[F[_] : Temporal : NConsole, State <: BuildState](
                                                                                 ongoing: Option[SlideAndTransition[F]],
                                                                                 sat: List[SlideAndTransition[F]]
                                                                               ) {
  def addSlide(slide: Slide[F]): PresentationBuilder[F, InProgress] =
    PresentationBuilder[F, InProgress](
      Option(SlideAndTransition(None, slide, None)),
      ongoing.fold(sat)(_ :: sat)
    )

  def build()(implicit ev: State <:< Started): F[Presentation[F]] =
    Presentation.make[F](ongoing.fold(sat)(_ :: sat).reverse)

  def addTransitions(
                      left: Option[Transition[F]] = None,
                      right: Option[Transition[F]] = None
                    )(implicit ev: State =:= InProgress): PresentationBuilder[F, Started] = {
    PresentationBuilder[F, Started](
      None,
      ongoing.fold(sat)(_.copy(left = left, right = right) :: sat)
    )
  }

}

object PresentationBuilder {
  def apply[F[_] : Temporal : NConsole](): PresentationBuilder[F, NotStarted] =
    PresentationBuilder[F, NotStarted](None, List.empty)
}

