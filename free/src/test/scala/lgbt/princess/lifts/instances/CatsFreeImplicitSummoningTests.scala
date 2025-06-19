package lgbt.princess.lifts
package instances

import cats.free.Free
import lgbt.princess.lifts.instances.catsFree._

object CatsFreeImplicitSummoningTests {

  def `Free instances for Option and List`(): Unit = {
    LiftValue[Option, Free[Option, *]]
    LiftScope[Option, Free[Option, *]]
    LiftKind[Option, Free[Option, *]]
    MapK[Option, List, Free[Option, *], Free[List, *]]
    LiftScopeAlt[Option, Free[Option, *]]
    LiftKind1[Option, Free[Option, *]]
    LiftKind2[Option, List, Free[Option, *], Free[List, *]]
  }

  def `Free instances for abstract types`[F[_], G[_]](): Unit = {
    LiftValue[F, Free[F, *]]
    LiftScope[F, Free[F, *]]
    LiftKind[F, Free[F, *]]
    MapK[F, G, Free[F, *], Free[G, *]]
    LiftScopeAlt[F, Free[F, *]]
    LiftKind1[F, Free[F, *]]
    LiftKind2[F, G, Free[F, *], Free[G, *]]
  }

  def `LiftKind1 from LiftKind2 for Free`[F[_], G[_]]()(implicit
      lk2: LiftKind2[F, F, Free[G, *], Free[G, *]]
  ): Unit = {
    LiftKind1[F, Free[G, *]]
  }
}
