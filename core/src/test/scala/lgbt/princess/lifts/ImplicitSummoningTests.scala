package lgbt.princess.lifts

import cats.Applicative
import cats.data.*

object ImplicitSummoningTests {

  def `instances for Option and List`(): Unit = {
    // LiftValue
    LiftValue[Option, Option]
    LiftValue[Option, OptionT[Option, *]]
    LiftValue[Option, EitherT[Option, Int, *]]
    LiftValue[Option, IorT[Option, Int, *]]
    LiftValue[Option, Kleisli[Option, Int, *]]
    LiftValue[Option, StateT[Option, Int, *]]
    LiftValue[Option, WriterT[Option, Int, *]]

    // LiftScope
    LiftScope[Option, Option]
    LiftScope[Option, OptionT[Option, *]]
    LiftScope[Option, EitherT[Option, Int, *]]
    LiftScope[Option, IorT[Option, Int, *]]
    LiftScope[Option, Kleisli[Option, Int, *]]
    LiftScope[Option, StateT[Option, Int, *]] // TODO: fix
    LiftScope[Option, WriterT[Option, Int, *]]

    // LiftKind
    LiftKind[Option, Option]
    LiftKind[Option, OptionT[Option, *]]
    LiftKind[Option, EitherT[Option, Int, *]]
    LiftKind[Option, IorT[Option, Int, *]]
    LiftKind[Option, Kleisli[Option, Int, *]]
    LiftKind[Option, StateT[Option, Int, *]]
    LiftKind[Option, WriterT[Option, Int, *]]

    // MapK
    MapK[Option, List, Option, List] // identity
    MapK[Option, List, OptionT[Option, *], OptionT[List, *]]
    MapK.Derived[Option, List, OptionT]
    MapK[Option, List, EitherT[Option, Int, *], EitherT[List, Int, *]]
    MapK.Derived[Option, List, EitherT[*[?], Int, *]]
    MapK[Option, List, IorT[Option, Int, *], IorT[List, Int, *]]
    MapK.Derived[Option, List, IorT[*[?], Int, *]]
    MapK[Option, List, Kleisli[Option, Int, *], Kleisli[List, Int, *]]
    MapK.Derived[Option, List, Kleisli[*[?], Int, *]]
    MapK[Option, List, StateT[Option, Int, *], StateT[List, Int, *]]
    MapK.Derived[Option, List, StateT[*[?], Int, *]]
    MapK[Option, List, WriterT[Option, Int, *], WriterT[List, Int, *]]
    MapK.Derived[Option, List, WriterT[*[?], Int, *]]

    // LiftScopeAlt
    LiftScopeAlt[Option, Option]
    LiftScopeAlt[Option, OptionT[Option, *]]
    LiftScopeAlt[Option, EitherT[Option, Int, *]]
    LiftScopeAlt[Option, IorT[Option, Int, *]]
    LiftScopeAlt[Option, Kleisli[Option, Int, *]]
    LiftScopeAlt[Option, StateT[Option, Int, *]]
    LiftScopeAlt[Option, WriterT[Option, Int, *]]

    // LiftKind1
    LiftKind1[Option, Option]
    LiftKind1[Option, OptionT[Option, *]]
    LiftKind1[Option, EitherT[Option, Int, *]]
    LiftKind1[Option, IorT[Option, Int, *]]
    LiftKind1[Option, Kleisli[Option, Int, *]]
    LiftKind1[Option, StateT[Option, Int, *]]
    LiftKind1[Option, WriterT[Option, Int, *]]

    // LiftKind2
    LiftKind2[Option, List, Option, List] // identity
    LiftKind2[Option, List, OptionT[Option, *], OptionT[List, *]]
    LiftKind2.Derived[Option, List, OptionT]
    LiftKind2[Option, List, EitherT[Option, Int, *], EitherT[List, Int, *]]
    LiftKind2.Derived[Option, List, EitherT[*[?], Int, *]]
    LiftKind2[Option, List, IorT[Option, Int, *], IorT[List, Int, *]]
    LiftKind2.Derived[Option, List, IorT[*[?], Int, *]]
    LiftKind2[Option, List, Kleisli[Option, Int, *], Kleisli[List, Int, *]]
    LiftKind2.Derived[Option, List, Kleisli[*[?], Int, *]]
    LiftKind2[Option, List, StateT[Option, Int, *], StateT[List, Int, *]]
    LiftKind2.Derived[Option, List, StateT[*[?], Int, *]]
    LiftKind2[Option, List, WriterT[Option, Int, *], WriterT[List, Int, *]]
    LiftKind2.Derived[Option, List, WriterT[*[?], Int, *]]
  }

  def `instances for abstract types`[F[_]: Applicative, G[_]: Applicative](): Unit = {
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
    MapK.Derived[F, G, EitherT[*[?], Int, *]]
    MapK[F, G, IorT[F, Int, *], IorT[G, Int, *]]
    MapK.Derived[F, G, IorT[*[?], Int, *]]
    MapK[F, G, Kleisli[F, Int, *], Kleisli[G, Int, *]]
    MapK.Derived[F, G, Kleisli[*[?], Int, *]]
    MapK[F, G, StateT[F, Int, *], StateT[G, Int, *]]
    MapK.Derived[F, G, StateT[*[?], Int, *]]
    MapK[F, G, WriterT[F, Int, *], WriterT[G, Int, *]]
    MapK.Derived[F, G, WriterT[*[?], Int, *]]

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
    LiftKind2.Derived[F, G, EitherT[*[?], Int, *]]
    LiftKind2[F, G, IorT[F, Int, *], IorT[G, Int, *]]
    LiftKind2.Derived[F, G, IorT[*[?], Int, *]]
    LiftKind2[F, G, Kleisli[F, Int, *], Kleisli[G, Int, *]]
    LiftKind2.Derived[F, G, Kleisli[*[?], Int, *]]
    LiftKind2[F, G, StateT[F, Int, *], StateT[G, Int, *]]
    LiftKind2.Derived[F, G, StateT[*[?], Int, *]]
    LiftKind2[F, G, WriterT[F, Int, *], WriterT[G, Int, *]]
    LiftKind2.Derived[F, G, WriterT[*[?], Int, *]]
  }

  def `instances from LiftKind2 of 4 abstract types`[F[_], G[_], H[_], I[_]]()(implicit
      lk2: LiftKind2[F, G, H, I]
  ): Unit = {
    LiftValue[F, H]
    LiftValue[G, I]
    MapK[F, G, H, I] // via inheritance
  }

  def `instances from LiftKind2 of 2 abstract types`[F[_], G[_]]()(implicit
      lk2: LiftKind2[F, F, G, G]
  ): Unit = {
    LiftValue[F, G]
    LiftKind1[F, G]
    LiftScopeAlt[F, G] // via inheritance
  }
}
