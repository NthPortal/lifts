package lgbt.princess.lifts

import cats.arrow.FunctionK
import cats.data.{EitherT, IorT, Kleisli, OptionT, WriterT}
import cats.{Applicative, Functor, Monoid, ~>}

import scala.annotation.implicitNotFound

/**
 * Lifts values and scope transformations from the higher-kinded type `F` to the higher-kinded type
 * `G`.
 */
@implicitNotFound("no way defined to lift values and scopes from ${F} to ${G}")
trait LiftKind[F[_], G[_]] extends LiftValue[F, G] with LiftScope[F, G] {
  def andThen[H[_]](that: LiftKind[G, H]): LiftKind[F, H] =
    LiftKind.Composed(this, that)

  def compose[E[_]](that: LiftKind[E, F]): LiftKind[E, G] =
    LiftKind.Composed(that, this)
}

object LiftKind {

  private[this] final class Composed[F[_], G[_], H[_]] private (
      inner: LiftKind[F, G],
      outer: LiftKind[G, H]
  ) extends LiftKind[F, H] {
    def liftF[A](value: F[A]): H[A] = outer.liftF(inner.liftF(value))
    val liftK: F ~> H = inner.liftK.andThen(outer.liftK)
    def limitedMapK[A](value: H[A])(scope: F ~> F): H[A] =
      outer.limitedMapK(value)(inner.liftScope(scope))
    override def liftScope(scope: F ~> F): H ~> H =
      outer.liftScope(inner.liftScope(scope))
  }

  private object Composed {
    def apply[F[_], G[_], H[_]](inner: LiftKind[F, G], outer: LiftKind[G, H]): LiftKind[F, H] =
      if (inner.isInstanceOf[Identity]) outer.asInstanceOf[LiftKind[F, H]]
      else if (outer.isInstanceOf[Identity]) inner.asInstanceOf[LiftKind[F, H]]
      else new Composed(inner, outer)
  }

  def apply[F[_], G[_]](implicit lk: LiftKind[F, G]): LiftKind[F, G] = lk

  implicit def id[F[_]]: LiftKind[F, F] =
    new LiftKind[F, F] with Identity {
      def liftF[A](value: F[A]): F[A] = value
      val liftK: F ~> F = FunctionK.id
      def limitedMapK[A](value: F[A])(scope: F ~> F): F[A] = scope(value)
      override def liftScope(scope: F ~> F): F ~> F = scope
    }

  implicit def optionT[F[_], G[_]: Functor](implicit
      inner: LiftKind[F, G]
  ): LiftKind[F, OptionT[G, *]] =
    inner.andThen {
      new LiftKind[G, OptionT[G, *]] {
        def liftF[A](value: G[A]): OptionT[G, A] = OptionT.liftF(value)
        val liftK: G ~> OptionT[G, *] = OptionT.liftK
        def limitedMapK[A](value: OptionT[G, A])(scope: G ~> G): OptionT[G, A] =
          value.mapK(scope)
      }
    }

  implicit def eitherT[F[_], G[_]: Functor, L](implicit
      inner: LiftKind[F, G]
  ): LiftKind[F, EitherT[G, L, *]] =
    inner.andThen {
      new LiftKind[G, EitherT[G, L, *]] {
        def liftF[A](value: G[A]): EitherT[G, L, A] = EitherT.liftF(value)
        val liftK: G ~> EitherT[G, L, *] = EitherT.liftK
        def limitedMapK[A](value: EitherT[G, L, A])(scope: G ~> G): EitherT[G, L, A] =
          value.mapK(scope)
      }
    }

  implicit def iorT[F[_], G[_]: Functor, L](implicit
      inner: LiftKind[F, G]
  ): LiftKind[F, IorT[G, L, *]] =
    inner.andThen {
      new LiftKind[G, IorT[G, L, *]] {
        def liftF[A](value: G[A]): IorT[G, L, A] = IorT.right(value)
        val liftK: G ~> IorT[G, L, *] = IorT.liftK
        def limitedMapK[A](value: IorT[G, L, A])(scope: G ~> G): IorT[G, L, A] =
          value.mapK(scope)
      }
    }

  implicit def kleisli[F[_], G[_], A](implicit
      inner: LiftKind[F, G]
  ): LiftKind[F, Kleisli[G, A, *]] =
    inner.andThen {
      new LiftKind[G, Kleisli[G, A, *]] {
        def liftF[B](value: G[B]): Kleisli[G, A, B] = Kleisli.liftF(value)
        val liftK: G ~> Kleisli[G, A, *] = Kleisli.liftK
        def limitedMapK[B](value: Kleisli[G, A, B])(scope: G ~> G): Kleisli[G, A, B] =
          value.mapK(scope)
      }
    }

  implicit def writerT[F[_], G[_]: Applicative, L: Monoid](implicit
      inner: LiftKind[F, G]
  ): LiftKind[F, WriterT[G, L, *]] =
    inner.andThen {
      new LiftKind[G, WriterT[G, L, *]] {
        def liftF[A](value: G[A]): WriterT[G, L, A] = WriterT.liftF(value)
        val liftK: G ~> WriterT[G, L, *] = WriterT.liftK
        def limitedMapK[A](value: WriterT[G, L, A])(scope: G ~> G): WriterT[G, L, A] =
          value.mapK(scope)
      }
    }
}
