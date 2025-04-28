package lgbt.princess.lifts

import cats.data.{EitherT, IorT, Kleisli, OptionT, WriterT}
import cats.kernel.Monoid
import cats.{Applicative, Functor, ~>}

trait LiftKind2[F[_], G[_], H[_], I[_]] extends MapK[F, G, H, I] {
  def liftValue1: LiftValue[F, H]
  def liftValue2: LiftValue[G, I]

  def andThen[J[_], K[_]](that: LiftKind2[H, I, J, K]): LiftKind2[F, G, J, K] =
    LiftKind2.Composed(this, that)

  def compose[D[_], E[_]](that: LiftKind2[D, E, F, G]): LiftKind2[D, E, H, I] =
    LiftKind2.Composed(that, this)
}

object LiftKind2 {

  private[this] final class Composed[F[_], G[_], H[_], I[_], J[_], K[_]] private (
      inner: LiftKind2[F, G, H, I],
      outer: LiftKind2[H, I, J, K],
  ) extends LiftKind2[F, G, J, K] {
    val liftValue1: LiftValue[F, J] =
      inner.liftValue1.andThen(outer.liftValue1)
    val liftValue2: LiftValue[G, K] =
      inner.liftValue2.andThen(outer.liftValue2)
    def mapK[A](value: J[A])(f: F ~> G): K[A] =
      outer.mapK(value)(inner.liftFunctionK(f))
    override def liftFunctionK(f: F ~> G): J ~> K =
      outer.liftFunctionK(inner.liftFunctionK(f))
  }

  private object Composed {
    def apply[F[_], G[_], H[_], I[_], J[_], K[_]](
        inner: LiftKind2[F, G, H, I],
        outer: LiftKind2[H, I, J, K],
    ): LiftKind2[F, G, J, K] =
      if (inner.isInstanceOf[Identity]) outer.asInstanceOf[LiftKind2[F, G, J, K]]
      else if (outer.isInstanceOf[Identity]) inner.asInstanceOf[LiftKind2[F, G, J, K]]
      else new Composed(inner, outer)
  }

  def apply[F[_], G[_], H[_], I[_]](implicit lk2: LiftKind2[F, G, H, I]): LiftKind2[F, G, H, I] =
    lk2

  implicit def id[F[_], G[_]]: LiftKind2[F, G, F, G] =
    new LiftKind2[F, G, F, G] with Identity {
      val liftValue1: LiftValue[F, F] = LiftValue.id
      val liftValue2: LiftValue[G, G] = LiftValue.id
      def mapK[A](value: F[A])(f: F ~> G): G[A] = f(value)
      override def liftFunctionK(f: F ~> G): F ~> G = f
    }

  implicit def optionT[F[_], G[_], H[_]: Functor, I[_]: Functor](implicit
      inner: LiftKind2[F, G, H, I]
  ): LiftKind2[F, G, OptionT[H, *], OptionT[I, *]] =
    inner.andThen {
      new LiftKind2[H, I, OptionT[H, *], OptionT[I, *]] {
        val liftValue1: LiftValue[H, OptionT[H, *]] = LiftValue.optionT
        val liftValue2: LiftValue[I, OptionT[I, *]] = LiftValue.optionT
        def mapK[A](value: OptionT[H, A])(f: H ~> I): OptionT[I, A] =
          value.mapK(f)
      }
    }

  implicit def eitherT[F[_], G[_], H[_]: Functor, I[_]: Functor, L](implicit
      inner: LiftKind2[F, G, H, I]
  ): LiftKind2[F, G, EitherT[H, L, *], EitherT[I, L, *]] =
    inner.andThen {
      new LiftKind2[H, I, EitherT[H, L, *], EitherT[I, L, *]] {
        val liftValue1: LiftValue[H, EitherT[H, L, *]] = LiftValue.eitherT
        val liftValue2: LiftValue[I, EitherT[I, L, *]] = LiftValue.eitherT
        def mapK[A](value: EitherT[H, L, A])(f: H ~> I): EitherT[I, L, A] =
          value.mapK(f)
      }
    }

  implicit def iorT[F[_], G[_], H[_]: Functor, I[_]: Functor, L](implicit
      inner: LiftKind2[F, G, H, I]
  ): LiftKind2[F, G, IorT[H, L, *], IorT[I, L, *]] =
    inner.andThen {
      new LiftKind2[H, I, IorT[H, L, *], IorT[I, L, *]] {
        val liftValue1: LiftValue[H, IorT[H, L, *]] = LiftValue.iorT
        val liftValue2: LiftValue[I, IorT[I, L, *]] = LiftValue.iorT
        def mapK[A](value: IorT[H, L, A])(f: H ~> I): IorT[I, L, A] =
          value.mapK(f)
      }
    }

  implicit def kleisli[F[_], G[_], H[_], I[_], A](implicit
      inner: LiftKind2[F, G, H, I]
  ): LiftKind2[F, G, Kleisli[H, A, *], Kleisli[I, A, *]] =
    inner.andThen {
      new LiftKind2[H, I, Kleisli[H, A, *], Kleisli[I, A, *]] {
        val liftValue1: LiftValue[H, Kleisli[H, A, *]] = LiftValue.kleisli
        val liftValue2: LiftValue[I, Kleisli[I, A, *]] = LiftValue.kleisli
        def mapK[B](value: Kleisli[H, A, B])(f: H ~> I): Kleisli[I, A, B] =
          value.mapK(f)
      }
    }

  implicit def writerT[F[_], G[_], H[_]: Applicative, I[_]: Applicative, L: Monoid](implicit
      inner: LiftKind2[F, G, H, I]
  ): LiftKind2[F, G, WriterT[H, L, *], WriterT[I, L, *]] =
    inner.andThen {
      new LiftKind2[H, I, WriterT[H, L, *], WriterT[I, L, *]] {
        val liftValue1: LiftValue[H, WriterT[H, L, *]] = LiftValue.writerT
        val liftValue2: LiftValue[I, WriterT[I, L, *]] = LiftValue.writerT
        def mapK[A](value: WriterT[H, L, A])(f: H ~> I): WriterT[I, L, A] =
          value.mapK(f)
      }
    }
}
