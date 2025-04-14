package lgbt.princess.lifts

import cats.data.{EitherT, IorT, Kleisli, OptionT, StateT, WriterT}
import cats.{Functor, ~>}

import scala.annotation.unused

trait MapK[F[_], G[_], H[_], I[_]] {
  def mapK[A](value: H[A])(f: F ~> G): I[A]

  def liftFunctionK(f: F ~> G): H ~> I =
    new (H ~> I) {
      def apply[A](fa: H[A]): I[A] = mapK(fa)(f)
    }

  final def liftScope(scope: F ~> G)(implicit @unused ev0: F =:= G, @unused ev1: H =:= I): H ~> I =
    liftFunctionK(scope)
}

object MapK {

  def apply[F[_], G[_], H[_], I[_]](implicit mk: MapK[F, G, H, I]): MapK[F, G, H, I] = mk

  /**
   * A `MapK` where the types `H` and `I` are derived from `F` and `G` using the type-constructor
   * `W`.
   */
  type Derived[F[_], G[_], W[_[_], _]] = MapK[F, G, W[F, *], W[G, *]]

  object Derived {
    def apply[F[_], G[_], W[_[_], _]](implicit mk: Derived[F, G, W]): Derived[F, G, W] = mk
  }

  /** A partially-applied 3-parameter type, with the middle parameter applied. */
  type PA3[T[_[_], _, _], C] = { type λ[F[_], A] = T[F, C, A] }

  implicit def id[F[_], G[_]]: MapK[F, G, F, G] =
    new MapK[F, G, F, G] {
      def mapK[A](value: F[A])(f: F ~> G): G[A] = f(value)
      override def liftFunctionK(f: F ~> G): F ~> G = f
    }

  implicit def optionT[F[_], G[_]]: Derived[F, G, OptionT] =
    new Derived[F, G, OptionT] {
      def mapK[A](value: OptionT[F, A])(f: F ~> G): OptionT[G, A] =
        value.mapK(f)
    }

  implicit def eitherT[F[_], G[_], L]: Derived[F, G, PA3[EitherT, L]#λ] =
    new Derived[F, G, PA3[EitherT, L]#λ] {
      def mapK[A](value: EitherT[F, L, A])(f: F ~> G): EitherT[G, L, A] =
        value.mapK(f)
    }

  implicit def iorT[F[_], G[_], L]: Derived[F, G, PA3[IorT, L]#λ] =
    new Derived[F, G, PA3[IorT, L]#λ] {
      def mapK[A](value: IorT[F, L, A])(f: F ~> G): IorT[G, L, A] =
        value.mapK(f)
    }

  implicit def kleisli[F[_], G[_], A]: Derived[F, G, PA3[Kleisli, A]#λ] =
    new Derived[F, G, PA3[Kleisli, A]#λ] {
      def mapK[B](value: Kleisli[F, A, B])(f: F ~> G): Kleisli[G, A, B] =
        value.mapK(f)
    }

  // TODO: move to an inner object
  implicit def stateT[F[_]: Functor, G[_], S]: Derived[F, G, PA3[StateT, S]#λ] =
    new Derived[F, G, PA3[StateT, S]#λ] {
      def mapK[A](value: StateT[F, S, A])(f: F ~> G): StateT[G, S, A] =
        value.mapK(f)
    }

  implicit def writerT[F[_], G[_], L]: Derived[F, G, PA3[WriterT, L]#λ] =
    new Derived[F, G, PA3[WriterT, L]#λ] {
      def mapK[A](value: WriterT[F, L, A])(f: F ~> G): WriterT[G, L, A] =
        value.mapK(f)
    }
}
