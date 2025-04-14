package lgbt.princess.lifts
package instances

import cats.effect.{IO, LiftIO}
import cats.effect.kernel.{MonadCancel, Resource}
import lgbt.princess.lifts.instances.ce._

import scala.annotation.unused

object CEImplicitSummoningTests {
  @unused def test1[F[_], G[_]](implicit F: MonadCancel[F, ?], G: MonadCancel[G, ?]): Unit = {
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

  @unused def test2[F[_]: LiftIO](): Unit = {
    LiftValue[IO, F]
  }

  @unused def test3[F[_], G[_]](): Unit = {
    implicit def lk2: LiftKind2.Derived[F, F, Resource] = ???
    LiftKind1[F, Resource[F, *]]
  }
}
