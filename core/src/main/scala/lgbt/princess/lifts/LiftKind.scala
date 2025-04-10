package lgbt.princess.lifts

import cats.arrow.FunctionK
import cats.data.{EitherT, IorT, Kleisli, OptionT, StateT, WriterT}
import cats.{Applicative, Functor, Monoid, ~>}

import scala.annotation.implicitNotFound

/**
 * Lifts values and scope transformations from the higher-kinded type `F` to the higher-kinded
 * type `G`.
 *
 * TODO: document issue with `StateT` implicit
 */
@implicitNotFound("no way defined to lift values and scopes from ${F} to ${G}")
trait LiftKind[F[_], G[_]] extends LiftValue[F, G] with LiftScope[F, G]

object LiftKind {
  implicit def id[F[_]]: LiftKind[F, F] =
    new LiftKind[F, F] {
      def liftF[A](value: F[A]): F[A] = value
      def liftK: F ~> F = FunctionK.id
      def liftScopeApply[A](scope: F ~> F)(value: F[A]): F[A] = scope(value)
      override def liftScope(scope: F ~> F): F ~> F = scope
    }

  implicit def optionT[F[_]: Functor]: LiftKind[F, OptionT[F, *]] =
    new LiftKind[F, OptionT[F, *]] {
      def liftF[A](value: F[A]): OptionT[F, A] = OptionT.liftF(value)
      val liftK: F ~> OptionT[F, *] = OptionT.liftK
      def liftScopeApply[A](scope: F ~> F)(value: OptionT[F, A]): OptionT[F, A] =
        value.mapK(scope)
    }

  implicit def eitherT[F[_]: Functor, L]: LiftKind[F, EitherT[F, L, *]] =
    new LiftKind[F, EitherT[F, L, *]] {
      def liftF[A](value: F[A]): EitherT[F, L, A] = EitherT.liftF(value)
      val liftK: F ~> EitherT[F, L, *] = EitherT.liftK
      def liftScopeApply[A](scope: F ~> F)(value: EitherT[F, L, A]): EitherT[F, L, A] =
        value.mapK(scope)
    }

  implicit def iorT[F[_]: Functor, L]: LiftKind[F, IorT[F, L, *]] =
    new LiftKind[F, IorT[F, L, *]] {
      def liftF[A](value: F[A]): IorT[F, L, A] = IorT.right(value)
      val liftK: F ~> IorT[F, L, *] = IorT.liftK
      def liftScopeApply[A](scope: F ~> F)(value: IorT[F, L, A]): IorT[F, L, A] =
        value.mapK(scope)
    }

  implicit def kleisli[F[_], A]: LiftKind[F, Kleisli[F, A, *]] =
    new LiftKind[F, Kleisli[F, A, *]] {
      def liftF[B](value: F[B]): Kleisli[F, A, B] = Kleisli.liftF(value)
      val liftK: F ~> Kleisli[F, A, *] = Kleisli.liftK
      def liftScopeApply[B](scope: F ~> F)(value: Kleisli[F, A, B]): Kleisli[F, A, B] =
        value.mapK(scope)
    }

  // TODO: move to an inner object
  implicit def stateT[F[_]: Applicative, S]: LiftKind[F, StateT[F, S, *]] =
    new LiftKind[F, StateT[F, S, *]] {
      def liftF[A](value: F[A]): StateT[F, S, A] = StateT.liftF(value)
      val liftK: F ~> StateT[F, S, *] = StateT.liftK
      def liftScopeApply[A](scope: F ~> F)(value: StateT[F, S, A]): StateT[F, S, A] =
        value.mapK(scope)
    }

  implicit def writerT[F[_]: Applicative, L: Monoid]: LiftKind[F, WriterT[F, L, *]] =
    new LiftKind[F, WriterT[F, L, *]] {
      def liftF[A](value: F[A]): WriterT[F, L, A] = WriterT.liftF(value)
      val liftK: F ~> WriterT[F, L, *] = WriterT.liftK
      def liftScopeApply[A](scope: F ~> F)(value: WriterT[F, L, A]): WriterT[F, L, A] =
        value.mapK(scope)
    }
}
