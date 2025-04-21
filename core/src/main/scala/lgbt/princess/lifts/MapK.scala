package lgbt.princess.lifts

import cats.data.{EitherT, IorT, Kleisli, OptionT, StateT, WriterT}
import cats.{Functor, ~>}

trait MapK[F[_], G[_], H[_], I[_]] {
  def mapK[A](value: H[A])(f: F ~> G): I[A]

  def liftFunctionK(f: F ~> G): H ~> I =
    new (H ~> I) {
      def apply[A](fa: H[A]): I[A] = mapK(fa)(f)
    }

  def andThen[J[_], K[_]](that: MapK[H, I, J, K]): MapK[F, G, J, K] =
    MapK.Composed(this, that)

  def compose[D[_], E[_]](that: MapK[D, E, F, G]): MapK[D, E, H, I] =
    MapK.Composed(that, this)
}

object MapK {

  private[this] final class Composed[F[_], G[_], H[_], I[_], J[_], K[_]] private (
      inner: MapK[F, G, H, I],
      outer: MapK[H, I, J, K],
  ) extends MapK[F, G, J, K] {
    def mapK[A](value: J[A])(f: F ~> G): K[A] =
      outer.mapK(value)(inner.liftFunctionK(f))
    override def liftFunctionK(f: F ~> G): J ~> K =
      outer.liftFunctionK(inner.liftFunctionK(f))
  }

  private object Composed {
    def apply[F[_], G[_], H[_], I[_], J[_], K[_]](
        inner: MapK[F, G, H, I],
        outer: MapK[H, I, J, K],
    ): MapK[F, G, J, K] = {
      if (inner.isInstanceOf[Identity]) outer.asInstanceOf[MapK[F, G, J, K]]
      else if (outer.isInstanceOf[Identity]) inner.asInstanceOf[MapK[F, G, J, K]]
      else new Composed(inner, outer)
    }
  }

  def apply[F[_], G[_], H[_], I[_]](implicit mk: MapK[F, G, H, I]): MapK[F, G, H, I] = mk

  implicit def id[F[_], G[_]]: MapK[F, G, F, G] =
    new MapK[F, G, F, G] with Identity {
      def mapK[A](value: F[A])(f: F ~> G): G[A] = f(value)
      override def liftFunctionK(f: F ~> G): F ~> G = f
    }

  implicit def optionT[F[_], G[_], H[_], I[_]](implicit
      inner: MapK[F, G, H, I]
  ): MapK[F, G, OptionT[H, *], OptionT[I, *]] =
    inner.andThen {
      new MapK[H, I, OptionT[H, *], OptionT[I, *]] {
        def mapK[A](value: OptionT[H, A])(f: H ~> I): OptionT[I, A] =
          value.mapK(f)
      }
    }

  implicit def eitherT[F[_], G[_], H[_], I[_], L](implicit
      inner: MapK[F, G, H, I]
  ): MapK[F, G, EitherT[H, L, *], EitherT[I, L, *]] =
    inner.andThen {
      new MapK[H, I, EitherT[H, L, *], EitherT[I, L, *]] {
        def mapK[A](value: EitherT[H, L, A])(f: H ~> I): EitherT[I, L, A] =
          value.mapK(f)
      }
    }

  implicit def iorT[F[_], G[_], H[_], I[_], L](implicit
      inner: MapK[F, G, H, I]
  ): MapK[F, G, IorT[H, L, *], IorT[I, L, *]] =
    inner.andThen {
      new MapK[H, I, IorT[H, L, *], IorT[I, L, *]] {
        def mapK[A](value: IorT[H, L, A])(f: H ~> I): IorT[I, L, A] =
          value.mapK(f)
      }
    }

  implicit def kleisli[F[_], G[_], H[_], I[_], A](implicit
      inner: MapK[F, G, H, I]
  ): MapK[F, G, Kleisli[H, A, *], Kleisli[I, A, *]] =
    inner.andThen {
      new MapK[H, I, Kleisli[H, A, *], Kleisli[I, A, *]] {
        def mapK[B](value: Kleisli[H, A, B])(f: H ~> I): Kleisli[I, A, B] =
          value.mapK(f)
      }
    }

  // TODO: move to an inner object
  implicit def stateT[F[_], G[_], H[_]: Functor, I[_], S](implicit
      inner: MapK[F, G, H, I]
  ): MapK[F, G, StateT[H, S, *], StateT[I, S, *]] =
    inner.andThen {
      new MapK[H, I, StateT[H, S, *], StateT[I, S, *]] {
        def mapK[A](value: StateT[H, S, A])(f: H ~> I): StateT[I, S, A] =
          value.mapK(f)
      }
    }

  implicit def writerT[F[_], G[_], H[_], I[_], L](implicit
      inner: MapK[F, G, H, I]
  ): MapK[F, G, WriterT[H, L, *], WriterT[I, L, *]] =
    inner.andThen {
      new MapK[H, I, WriterT[H, L, *], WriterT[I, L, *]] {
        def mapK[A](value: WriterT[H, L, A])(f: H ~> I): WriterT[I, L, A] =
          value.mapK(f)
      }
    }
}
