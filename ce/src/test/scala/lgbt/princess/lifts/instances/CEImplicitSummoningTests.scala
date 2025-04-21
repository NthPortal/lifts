package lgbt.princess.lifts
package instances

import cats.data.OptionT
import cats.effect.{IO, LiftIO}
import cats.effect.kernel.{MonadCancel, Resource}
import lgbt.princess.lifts.instances.ce._

object CEImplicitSummoningTests {

  def `Resource instances for IO`(): Unit = {
//    LiftValue[IO, Resource[IO, *]] // TODO: fix ambiguity between LiftIO and Resource instances
    LiftScope[IO, Resource[IO, *]]
    LiftKind[IO, Resource[IO, *]]
    MapK[IO, OptionT[IO, *], Resource[IO, *], Resource[OptionT[IO, *], *]]
    LiftScopeAlt[IO, Resource[IO, *]]
    LiftKind1[IO, Resource[IO, *]]
    LiftKind2[IO, OptionT[IO, *], Resource[IO, *], Resource[OptionT[IO, *], *]]
  }

  def `Resource instances for abstract types`[F[_], G[_]]()(implicit
      F: MonadCancel[F, ?],
      G: MonadCancel[G, ?]
  ): Unit = {
    LiftValue[F, Resource[F, *]]
    LiftScope[F, Resource[F, *]]
    LiftKind[F, Resource[F, *]]
    MapK[F, G, Resource[F, *], Resource[G, *]]
    LiftScopeAlt[F, Resource[F, *]]
    LiftKind1[F, Resource[F, *]]
    LiftKind2[F, G, Resource[F, *], Resource[G, *]]
  }

  def `LiftValue for IO`(): Unit = {
    LiftValue[IO, IO] // why isn't this ambiguous with identity?
  }

  def `LiftValue for IO and abstract type`[F[_]: LiftIO](): Unit = {
    LiftValue[IO, F]
  }

  def `LiftKind1 from LiftKind2 for Resource`[F[_]]()(implicit
      lk2: LiftKind2[F, F, Resource[F, *], Resource[F, *]]
  ): Unit = {
    LiftKind1[F, Resource[F, *]]
  }
}
