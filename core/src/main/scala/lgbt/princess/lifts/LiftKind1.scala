package lgbt.princess.lifts

import cats.arrow.FunctionK
import cats.data.{EitherT, IorT, Kleisli, OptionT, StateT, WriterT}
import cats.{Applicative, Functor, Monoid, ~>}

trait LiftKind1[F[_], G[_]] extends LiftValue[F, G] with LiftScopeAlt[F, G] {
  final def liftScope(scope: F ~> F): G ~> G = liftFunctionK(scope)

  def andThen[H[_]](that: LiftKind1[G, H]): LiftKind1[F, H] =
    LiftKind1.Composed(this, that)

  def compose[E[_]](that: LiftKind1[E, F]): LiftKind1[E, G] =
    LiftKind1.Composed(that, this)
}

object LiftKind1 extends LowPriorityLiftKind1Implicits {

  private[this] final class Composed[F[_], G[_], H[_]] private (
      inner: LiftKind1[F, G],
      outer: LiftKind1[G, H]
  ) extends LiftKind1[F, H] {
    def liftF[A](value: F[A]): H[A] = outer.liftF(inner.liftF(value))
    val liftK: F ~> H = inner.liftK.andThen(outer.liftK)
    def mapK[A](value: H[A])(f: F ~> F): H[A] =
      outer.mapK(value)(inner.liftFunctionK(f))
    override def liftFunctionK(f: F ~> F): H ~> H =
      outer.liftFunctionK(inner.liftFunctionK(f))
  }

  private object Composed {
    def apply[F[_], G[_], H[_]](inner: LiftKind1[F, G], outer: LiftKind1[G, H]): LiftKind1[F, H] =
      if (inner.isInstanceOf[Identity]) outer.asInstanceOf[LiftKind1[F, H]]
      else if (outer.isInstanceOf[Identity]) inner.asInstanceOf[LiftKind1[F, H]]
      else new Composed(inner, outer)
  }

  def apply[F[_], G[_]](implicit lk1: LiftKind1[F, G]): LiftKind1[F, G] = lk1

  implicit def id[F[_]]: LiftKind1[F, F] =
    new LiftKind1[F, F] with Identity {
      def liftF[A](value: F[A]): F[A] = value
      def liftK: F ~> F = FunctionK.id
      def mapK[A](value: F[A])(scope: F ~> F): F[A] = scope(value)
      override def liftFunctionK(scope: F ~> F): F ~> F = scope
    }

  implicit def optionT[F[_], G[_]: Functor](implicit
      inner: LiftKind1[F, G]
  ): LiftKind1[F, OptionT[G, *]] =
    inner.andThen {
      new LiftKind1[G, OptionT[G, *]] {
        def liftF[A](value: G[A]): OptionT[G, A] = OptionT.liftF(value)
        val liftK: G ~> OptionT[G, *] = OptionT.liftK
        def mapK[A](value: OptionT[G, A])(scope: G ~> G): OptionT[G, A] =
          value.mapK(scope)
      }
    }

  implicit def eitherT[F[_], G[_]: Functor, L](implicit
      inner: LiftKind1[F, G]
  ): LiftKind1[F, EitherT[G, L, *]] =
    inner.andThen {
      new LiftKind1[G, EitherT[G, L, *]] {
        def liftF[A](value: G[A]): EitherT[G, L, A] = EitherT.liftF(value)
        val liftK: G ~> EitherT[G, L, *] = EitherT.liftK
        def mapK[A](value: EitherT[G, L, A])(scope: G ~> G): EitherT[G, L, A] =
          value.mapK(scope)
      }
    }

  implicit def iorT[F[_], G[_]: Functor, L](implicit
      inner: LiftKind1[F, G]
  ): LiftKind1[F, IorT[G, L, *]] =
    inner.andThen {
      new LiftKind1[G, IorT[G, L, *]] {
        def liftF[A](value: G[A]): IorT[G, L, A] = IorT.right(value)
        val liftK: G ~> IorT[G, L, *] = IorT.liftK
        def mapK[A](value: IorT[G, L, A])(scope: G ~> G): IorT[G, L, A] =
          value.mapK(scope)
      }
    }

  implicit def kleisli[F[_], G[_], A](implicit
      inner: LiftKind1[F, G]
  ): LiftKind1[F, Kleisli[G, A, *]] =
    inner.andThen {
      new LiftKind1[G, Kleisli[G, A, *]] {
        def liftF[B](value: G[B]): Kleisli[G, A, B] = Kleisli.liftF(value)
        val liftK: G ~> Kleisli[G, A, *] = Kleisli.liftK
        def mapK[B](value: Kleisli[G, A, B])(scope: G ~> G): Kleisli[G, A, B] =
          value.mapK(scope)
      }
    }

  // TODO: move to an inner object
  implicit def stateT[F[_], G[_]: Applicative, S](implicit
      inner: LiftKind1[F, G]
  ): LiftKind1[F, StateT[G, S, *]] =
    inner.andThen {
      new LiftKind1[G, StateT[G, S, *]] {
        def liftF[A](value: G[A]): StateT[G, S, A] = StateT.liftF(value)
        val liftK: G ~> StateT[G, S, *] = StateT.liftK
        def mapK[A](value: StateT[G, S, A])(scope: G ~> G): StateT[G, S, A] =
          value.mapK(scope)
      }
    }

  implicit def writerT[F[_], G[_]: Applicative, L: Monoid](implicit
      inner: LiftKind1[F, G]
  ): LiftKind1[F, WriterT[G, L, *]] =
    inner.andThen {
      new LiftKind1[G, WriterT[G, L, *]] {
        def liftF[A](value: G[A]): WriterT[G, L, A] = WriterT.liftF(value)
        val liftK: G ~> WriterT[G, L, *] = WriterT.liftK
        def mapK[A](value: WriterT[G, L, A])(scope: G ~> G): WriterT[G, L, A] =
          value.mapK(scope)
      }
    }
}

trait LowPriorityLiftKind1Implicits {
  implicit def liftKind1FromLiftKind2[F[_], G[_]](implicit
      lk2: LiftKind2[F, F, G, G]
  ): LiftKind1[F, G] =
    new LiftKind1[F, G] {
      def liftF[A](value: F[A]): G[A] = lk2.liftValue1.liftF(value)
      def liftK: F ~> G = lk2.liftValue1.liftK
      def mapK[A](value: G[A])(f: F ~> F): G[A] = lk2.mapK(value)(f)
      override def liftFunctionK(f: F ~> F): G ~> G = lk2.liftFunctionK(f)
    }
}
