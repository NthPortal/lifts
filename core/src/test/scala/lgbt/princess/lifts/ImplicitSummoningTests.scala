package lgbt.princess.lifts

import cats.Applicative
import cats.data.*

import scala.annotation.unused

object ImplicitSummoningTests {
  @unused def test1[F[_]: Applicative, G[_]: Applicative](): Unit = {
    // LiftValue
    LiftValue[F, F]
    LiftValue[F, OptionT[F, *]]
    LiftValue[F, EitherT[F, Int, *]]
    LiftValue[F, IorT[F, Int, *]]
    LiftValue[F, Kleisli[F, Int, *]]
    LiftValue[F, StateT[F, Int, *]]
    LiftValue[F, WriterT[F, Int, *]]

    // LiftScope
    LiftScope[F, F]
    LiftScope[F, OptionT[F, *]]
    LiftScope[F, EitherT[F, Int, *]]
    LiftScope[F, IorT[F, Int, *]]
    LiftScope[F, Kleisli[F, Int, *]]
    LiftScope[F, StateT[F, Int, *]] // TODO: fix
    LiftScope[F, WriterT[F, Int, *]]

    // LiftKind
    LiftKind[F, F]
    LiftKind[F, OptionT[F, *]]
    LiftKind[F, EitherT[F, Int, *]]
    LiftKind[F, IorT[F, Int, *]]
    LiftKind[F, Kleisli[F, Int, *]]
    LiftKind[F, StateT[F, Int, *]]
    LiftKind[F, WriterT[F, Int, *]]

    // MapK
    MapK[F, G, F, G] // identity
    MapK[F, G, OptionT[F, *], OptionT[G, *]]
    MapK.Derived[F, G, OptionT]
    MapK[F, G, EitherT[F, Int, *], EitherT[G, Int, *]]
    MapK.Derived[F, G, MapK.PA3[EitherT, Int]#λ]
    MapK[F, G, IorT[F, Int, *], IorT[G, Int, *]]
    MapK.Derived[F, G, MapK.PA3[IorT, Int]#λ]
    MapK[F, G, Kleisli[F, Int, *], Kleisli[G, Int, *]]
    MapK.Derived[F, G, MapK.PA3[Kleisli, Int]#λ]
    MapK[F, G, StateT[F, Int, *], StateT[G, Int, *]]
    MapK.Derived[F, G, MapK.PA3[StateT, Int]#λ]
    MapK[F, G, WriterT[F, Int, *], WriterT[G, Int, *]]
    MapK.Derived[F, G, MapK.PA3[WriterT, Int]#λ]

    // LiftScopeAlt
    LiftScopeAlt[F, F]
    LiftScopeAlt[F, OptionT[F, *]]
    LiftScopeAlt[F, EitherT[F, Int, *]]
    LiftScopeAlt[F, IorT[F, Int, *]]
    LiftScopeAlt[F, Kleisli[F, Int, *]]
    LiftScopeAlt[F, StateT[F, Int, *]]
    LiftScopeAlt[F, WriterT[F, Int, *]]

    // LiftKind1
    LiftKind1[F, F]
    LiftKind1[F, OptionT[F, *]]
    LiftKind1[F, EitherT[F, Int, *]]
    LiftKind1[F, IorT[F, Int, *]]
    LiftKind1[F, Kleisli[F, Int, *]]
    LiftKind1[F, StateT[F, Int, *]]
    LiftKind1[F, WriterT[F, Int, *]]

    // LiftKind2
    LiftKind2[F, G, F, G] // identity
    LiftKind2[F, G, OptionT[F, *], OptionT[G, *]]
    LiftKind2.Derived[F, G, OptionT]
    LiftKind2[F, G, EitherT[F, Int, *], EitherT[G, Int, *]]
    LiftKind2.Derived[F, G, LiftKind2.PA3[EitherT, Int]#λ]
    LiftKind2[F, G, IorT[F, Int, *], IorT[G, Int, *]]
    LiftKind2.Derived[F, G, LiftKind2.PA3[IorT, Int]#λ]
    LiftKind2[F, G, Kleisli[F, Int, *], Kleisli[G, Int, *]]
    LiftKind2.Derived[F, G, LiftKind2.PA3[Kleisli, Int]#λ]
    LiftKind2[F, G, StateT[F, Int, *], StateT[G, Int, *]]
    LiftKind2.Derived[F, G, LiftKind2.PA3[StateT, Int]#λ]
    LiftKind2[F, G, WriterT[F, Int, *], WriterT[G, Int, *]]
    LiftKind2.Derived[F, G, LiftKind2.PA3[WriterT, Int]#λ]
  }

  @unused def test2[F[_], G[_], H[_], I[_]](): Unit = {
    implicit def lk2: LiftKind2[F, G, H, I] = ???
    LiftValue[F, H]
    LiftValue[G, I]
    MapK[F, G, H, I] // via inheritance
  }

  @unused def test3[F[_], G[_]](): Unit = {
    implicit def lk2: LiftKind2[F, F, G, G] = ???
    LiftKind1[F, G]
    LiftScopeAlt[F, G] // via inheritance
  }
}
