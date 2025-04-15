package lgbt.princess.lifts
package instances

import cats.data.OptionT
import cats.effect.{IO, LiftIO}
import cats.effect.kernel.{MonadCancel, Resource}
import lgbt.princess.lifts.instances.ce._

object CEImplicitSummoningTests {

  def `Resource instances for IO`(): Unit = {
    LiftValue[IO, Resource[IO, *]]
    LiftScope[IO, Resource[IO, *]]
    LiftKind[IO, Resource[IO, *]]
    MapK[IO, OptionT[IO, *], Resource[IO, *], Resource[OptionT[IO, *], *]]
    MapK.Derived[IO, OptionT[IO, *], Resource]
    LiftScopeAlt[IO, Resource[IO, *]]
    LiftKind1[IO, Resource[IO, *]]
    LiftKind2[IO, OptionT[IO, *], Resource[IO, *], Resource[OptionT[IO, *], *]]
    LiftKind2.Derived[IO, OptionT[IO, *], Resource]
  }

  def `Resource instances for abstract types`[F[_], G[_]]()(implicit
      F: MonadCancel[F, ?],
      G: MonadCancel[G, ?]
  ): Unit = {
    LiftValue[F, Resource[F, *]]
    LiftScope[F, Resource[F, *]]
    LiftKind[F, Resource[F, *]]
    MapK[F, G, Resource[F, *], Resource[G, *]]
    MapK.Derived[F, G, Resource]
    LiftScopeAlt[F, Resource[F, *]]
    LiftKind1[F, Resource[F, *]]
    LiftKind2[F, G, Resource[F, *], Resource[G, *]]
    LiftKind2.Derived[F, G, Resource]
  }

  def `LiftValue for IO`(): Unit = {
    LiftValue[IO, IO] // why isn't this ambiguous with identity?
  }

  def `LiftValue for IO and abstract type`[F[_]: LiftIO](): Unit = {
    LiftValue[IO, F]
  }

  def `LiftKind1 from LiftKind2 for Resource`[F[_]]()(implicit
      lk2: LiftKind2.Derived[F, F, Resource]
  ): Unit = {
    LiftKind1[F, Resource[F, *]]
  }
}
