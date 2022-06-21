package com.github.morotsman
package presentation.tools

import cats.effect.{Sync, Temporal}
import presentation.tools.transition.Transition

import presentation.tools.PresentationBuilder._
import presentation.tools.SimpleSlide.ToSimpleSlide

final case class SlideAndTransition[F[_]](
                                           left: Option[Transition[F]],
                                           slide: Slide[F],
                                           right: Option[Transition[F]]
                                         )




case class PresentationBuilder[F[_] : Temporal : NConsole: Sync, State <: BuildState](
                                                                                 ongoing: Option[SlideAndTransition[F]],
                                                                                 sat: List[SlideAndTransition[F]]
                                                                               ) {
  def addSlide(slide: Slide[F]): PresentationBuilder[F, State with SlideAdded] =
    PresentationBuilder(
      Option(SlideAndTransition(None, slide, None)),
      ongoing.fold(sat)(_ :: sat)
    )

  def addSlide(s: String): PresentationBuilder[F, State with SlideAdded] =
    addSlide(s.toSlide)

  def build()(implicit ev: State =:= Buildable): F[Presentation[F]] =
    Presentation.make[F](ongoing.fold(sat)(_ :: sat).reverse)

  def addTransitions(
                      left: Transition[F] = null,
                      right: Transition[F] = null
                    )(implicit ev: State <:< SlideAdded): PresentationBuilder[F, State] = {
    PresentationBuilder(
      None,
      ongoing.fold(sat)(_.copy(left = Option(left), right = Option(right)) :: sat)
    )
  }

}

object PresentationBuilder {
  sealed trait BuildState

  sealed trait Empty extends BuildState
  sealed trait SlideAdded extends BuildState

  type Buildable = Empty with SlideAdded

  def apply[F[_] : Temporal : NConsole: Sync](): PresentationBuilder[F, Empty] =
    PresentationBuilder[F, Empty](None, List.empty)
}

