package lgbt.princess.lifts

import cats.arrow.FunctionK
import cats.data.{EitherT, IorT, Kleisli, OptionT, StateT, WriterT}
import cats.{Applicative, Functor, Monoid, ~>}

trait LiftKind1[F[_], G[_]] extends LiftValue[F, G] with LiftScopeAlt[F, G] {
  final def liftScope(scope: F ~> F): G ~> G = liftFunctionK(scope)

  def andThen[H[_]](that: LiftKind1[G, H]): LiftKind1[F, H] =
    new LiftKind1.Composed(this, that)

  def compose[E[_]](that: LiftKind1[E, F]): LiftKind1[E, G] =
    new LiftKind1.Composed(that, this)
}

object LiftKind1 extends LowPriorityLiftKind1Implicits {

  private final class Composed[F[_], G[_], H[_]](inner: LiftKind1[F, G], outer: LiftKind1[G, H])
      extends LiftKind1[F, H] {
    def liftF[A](value: F[A]): H[A] = outer.liftF(inner.liftF(value))
    val liftK: F ~> H = inner.liftK.andThen(outer.liftK)
    def mapK[A](value: H[A])(f: F ~> F): H[A] =
      outer.mapK(value)(inner.liftFunctionK(f))
    override def liftFunctionK(f: F ~> F): H ~> H =
      outer.liftFunctionK(inner.liftFunctionK(f))
  }

  def apply[F[_], G[_]](implicit lk1: LiftKind1[F, G]): LiftKind1[F, G] = lk1

  implicit def id[F[_]]: LiftKind1[F, F] =
    new LiftKind1[F, F] {
      def liftF[A](value: F[A]): F[A] = value
      def liftK: F ~> F = FunctionK.id
      def mapK[A](value: F[A])(scope: F ~> F): F[A] = scope(value)
      override def liftFunctionK(scope: F ~> F): F ~> F = scope
    }

  implicit def optionT[F[_]: Functor]: LiftKind1[F, OptionT[F, *]] =
    new LiftKind1[F, OptionT[F, *]] {
      def liftF[A](value: F[A]): OptionT[F, A] = OptionT.liftF(value)
      val liftK: F ~> OptionT[F, *] = OptionT.liftK
      def mapK[A](value: OptionT[F, A])(scope: F ~> F): OptionT[F, A] =
        value.mapK(scope)
    }

  implicit def eitherT[F[_]: Functor, L]: LiftKind1[F, EitherT[F, L, *]] =
    new LiftKind1[F, EitherT[F, L, *]] {
      def liftF[A](value: F[A]): EitherT[F, L, A] = EitherT.liftF(value)
      val liftK: F ~> EitherT[F, L, *] = EitherT.liftK
      def mapK[A](value: EitherT[F, L, A])(scope: F ~> F): EitherT[F, L, A] =
        value.mapK(scope)
    }

  implicit def iorT[F[_]: Functor, L]: LiftKind1[F, IorT[F, L, *]] =
    new LiftKind1[F, IorT[F, L, *]] {
      def liftF[A](value: F[A]): IorT[F, L, A] = IorT.right(value)
      val liftK: F ~> IorT[F, L, *] = IorT.liftK
      def mapK[A](value: IorT[F, L, A])(scope: F ~> F): IorT[F, L, A] =
        value.mapK(scope)
    }

  implicit def kleisli[F[_], A]: LiftKind1[F, Kleisli[F, A, *]] =
    new LiftKind1[F, Kleisli[F, A, *]] {
      def liftF[B](value: F[B]): Kleisli[F, A, B] = Kleisli.liftF(value)
      val liftK: F ~> Kleisli[F, A, *] = Kleisli.liftK
      def mapK[B](value: Kleisli[F, A, B])(scope: F ~> F): Kleisli[F, A, B] =
        value.mapK(scope)
    }

  // TODO: move to an inner object
  implicit def stateT[F[_]: Applicative, S]: LiftKind1[F, StateT[F, S, *]] =
    new LiftKind1[F, StateT[F, S, *]] {
      def liftF[A](value: F[A]): StateT[F, S, A] = StateT.liftF(value)
      val liftK: F ~> StateT[F, S, *] = StateT.liftK
      def mapK[A](value: StateT[F, S, A])(scope: F ~> F): StateT[F, S, A] =
        value.mapK(scope)
    }

  implicit def writerT[F[_]: Applicative, L: Monoid]: LiftKind1[F, WriterT[F, L, *]] =
    new LiftKind1[F, WriterT[F, L, *]] {
      def liftF[A](value: F[A]): WriterT[F, L, A] = WriterT.liftF(value)
      val liftK: F ~> WriterT[F, L, *] = WriterT.liftK
      def mapK[A](value: WriterT[F, L, A])(scope: F ~> F): WriterT[F, L, A] =
        value.mapK(scope)
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
