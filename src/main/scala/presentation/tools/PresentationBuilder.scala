package com.github.morotsman
package presentation.tools

import cats.effect.{Sync, Temporal}
import presentation.tools.transition.Transition

import com.github.morotsman.presentation.tools.SimpleSlide.ToSimpleSlide

final case class SlideAndTransition[F[_]](
                                           left: Option[Transition[F]],
                                           slide: Slide[F],
                                           right: Option[Transition[F]]
                                         )


sealed trait BuildState

sealed trait NotStarted extends BuildState

sealed trait SlideAdded extends BuildState

case class PresentationBuilder[F[_] : Temporal : NConsole: Sync, State <: BuildState](
                                                                                 ongoing: Option[SlideAndTransition[F]],
                                                                                 sat: List[SlideAndTransition[F]]
                                                                               ) {
  def addSlide(slide: Slide[F]): PresentationBuilder[F, SlideAdded] =
    PresentationBuilder[F, SlideAdded](
      Option(SlideAndTransition(None, slide, None)),
      ongoing.fold(sat)(_ :: sat)
    )

  def addSlide(s: String): PresentationBuilder[F, SlideAdded] =
    addSlide(s.toSlide)

  def build()(implicit ev: State <:< SlideAdded): F[Presentation[F]] =
    Presentation.make[F](ongoing.fold(sat)(_ :: sat).reverse)

  def addTransitions(
                      left: Transition[F] = null,
                      right: Transition[F] = null
                    )(implicit ev: State =:= SlideAdded): PresentationBuilder[F, SlideAdded] = {
    PresentationBuilder[F, SlideAdded](
      None,
      ongoing.fold(sat)(_.copy(left = Option(left), right = Option(right)) :: sat)
    )
  }

}

object PresentationBuilder {
  def apply[F[_] : Temporal : NConsole: Sync](): PresentationBuilder[F, NotStarted] =
    PresentationBuilder[F, NotStarted](None, List.empty)
}

