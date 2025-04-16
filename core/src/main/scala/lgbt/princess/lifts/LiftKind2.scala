package lgbt.princess.lifts

import cats.data.{EitherT, IorT, Kleisli, OptionT, StateT, WriterT}
import cats.kernel.Monoid
import cats.{Applicative, Functor, ~>}

trait LiftKind2[F[_], G[_], H[_], I[_]] extends MapK[F, G, H, I] {
  def liftValue1: LiftValue[F, H]
  def liftValue2: LiftValue[G, I]
}

object LiftKind2 {

  def apply[F[_], G[_], H[_], I[_]](implicit lk2: LiftKind2[F, G, H, I]): LiftKind2[F, G, H, I] =
    lk2

  /**
   * A `LiftKind2` where the types `H` and `I` are derived from `F` and `G` using the
   * type-constructor `W`.
   */
  type Derived[F[_], G[_], W[_[_], _]] = LiftKind2[F, G, W[F, *], W[G, *]]

  object Derived {
    def apply[F[_], G[_], W[_[_], _]](implicit lk2: Derived[F, G, W]): Derived[F, G, W] = lk2
  }

  implicit def id[F[_], G[_]]: LiftKind2[F, G, F, G] =
    new LiftKind2[F, G, F, G] {
      val liftValue1: LiftValue[F, F] = LiftValue.id
      val liftValue2: LiftValue[G, G] = LiftValue.id
      def mapK[A](value: F[A])(f: F ~> G): G[A] = f(value)
      override def liftFunctionK(f: F ~> G): F ~> G = f
    }

  implicit def optionT[F[_]: Functor, G[_]: Functor]: Derived[F, G, OptionT] =
    new Derived[F, G, OptionT] {
      val liftValue1: LiftValue[F, OptionT[F, *]] = LiftValue.optionT
      val liftValue2: LiftValue[G, OptionT[G, *]] = LiftValue.optionT
      def mapK[A](value: OptionT[F, A])(f: F ~> G): OptionT[G, A] =
        value.mapK(f)
    }

  implicit def eitherT[F[_]: Functor, G[_]: Functor, L]: Derived[F, G, EitherT[*[?], L, *]] =
    new Derived[F, G, EitherT[*[?], L, *]] {
      val liftValue1: LiftValue[F, EitherT[F, L, *]] = LiftValue.eitherT
      val liftValue2: LiftValue[G, EitherT[G, L, *]] = LiftValue.eitherT
      def mapK[A](value: EitherT[F, L, A])(f: F ~> G): EitherT[G, L, A] =
        value.mapK(f)
    }

  implicit def iorT[F[_]: Functor, G[_]: Functor, L]: Derived[F, G, IorT[*[?], L, *]] =
    new Derived[F, G, IorT[*[?], L, *]] {
      val liftValue1: LiftValue[F, IorT[F, L, *]] = LiftValue.iorT
      val liftValue2: LiftValue[G, IorT[G, L, *]] = LiftValue.iorT
      def mapK[A](value: IorT[F, L, A])(f: F ~> G): IorT[G, L, A] =
        value.mapK(f)
    }

  implicit def kleisli[F[_], G[_], A]: Derived[F, G, Kleisli[*[?], A, *]] =
    new Derived[F, G, Kleisli[*[?], A, *]] {
      val liftValue1: LiftValue[F, Kleisli[F, A, *]] = LiftValue.kleisli
      val liftValue2: LiftValue[G, Kleisli[G, A, *]] = LiftValue.kleisli
      def mapK[B](value: Kleisli[F, A, B])(f: F ~> G): Kleisli[G, A, B] =
        value.mapK(f)
    }

  // TODO: move to an inner object
  implicit def stateT[F[_]: Applicative, G[_]: Applicative, S]: Derived[F, G, StateT[*[?], S, *]] =
    new Derived[F, G, StateT[*[?], S, *]] {
      val liftValue1: LiftValue[F, StateT[F, S, *]] = LiftValue.stateT
      val liftValue2: LiftValue[G, StateT[G, S, *]] = LiftValue.stateT
      def mapK[A](value: StateT[F, S, A])(f: F ~> G): StateT[G, S, A] =
        value.mapK(f)
    }

  implicit def writerT[F[_]: Applicative, G[_]: Applicative, L: Monoid]
      : Derived[F, G, WriterT[*[?], L, *]] =
    new Derived[F, G, WriterT[*[?], L, *]] {
      val liftValue1: LiftValue[F, WriterT[F, L, *]] = LiftValue.writerT
      val liftValue2: LiftValue[G, WriterT[G, L, *]] = LiftValue.writerT
      def mapK[A](value: WriterT[F, L, A])(f: F ~> G): WriterT[G, L, A] =
        value.mapK(f)
    }
}
