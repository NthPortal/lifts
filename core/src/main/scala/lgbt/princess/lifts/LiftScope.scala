package lgbt.princess.lifts

import cats.data.{EitherT, IorT, Kleisli, OptionT, StateT, WriterT}
import cats.{Functor, ~>}

import scala.annotation.implicitNotFound

/**
 * Lifts scope transformations from the higher-kinded type `F` to the higher-kinded type `G`.
 *
 * TODO: document issue with `StateT` implicit
 */
@implicitNotFound("no way defined to lift scopes from ${F} to ${G}")
trait LiftScope[F[_], G[_]] {

  /**
   * Modifies the context of `G[A]` using the given scope transformation in `F`.
   *
   * @note
   *   This method is usually best implemented by a `mapK` method on `G`.
   */
  def liftScopeApply[A](scope: F ~> F)(value: G[A]): G[A]

  /**
   * Lifts a scope transformation in `F` into a scope operation in `G`.
   *
   * @note
   *   Implementors SHOULD NOT override this method; the only reason it is not final is for
   *   optimization of the identity case.
   */
  def liftScope(scope: F ~> F): G ~> G =
    new (G ~> G) {
      def apply[A](fa: G[A]): G[A] = liftScopeApply(scope)(fa)
    }
}

object LiftScope {

  def apply[F[_], G[_]](implicit ls: LiftScope[F, G]): LiftScope[F, G] = ls

  implicit def id[F[_]]: LiftScope[F, F] =
    new LiftScope[F, F] {
      def liftScopeApply[A](scope: F ~> F)(value: F[A]): F[A] = scope(value)
      override def liftScope(scope: F ~> F): F ~> F = scope
    }

  implicit def optionT[F[_]]: LiftScope[F, OptionT[F, *]] =
    new LiftScope[F, OptionT[F, *]] {
      def liftScopeApply[A](scope: F ~> F)(value: OptionT[F, A]): OptionT[F, A] =
        value.mapK(scope)
    }

  implicit def eitherT[F[_], L]: LiftScope[F, EitherT[F, L, *]] =
    new LiftScope[F, EitherT[F, L, *]] {
      def liftScopeApply[A](scope: F ~> F)(value: EitherT[F, L, A]): EitherT[F, L, A] =
        value.mapK(scope)
    }

  implicit def iorT[F[_], L]: LiftScope[F, IorT[F, L, *]] =
    new LiftScope[F, IorT[F, L, *]] {
      def liftScopeApply[A](scope: F ~> F)(value: IorT[F, L, A]): IorT[F, L, A] =
        value.mapK(scope)
    }

  implicit def kleisli[F[_], A]: LiftScope[F, Kleisli[F, A, *]] =
    new LiftScope[F, Kleisli[F, A, *]] {
      def liftScopeApply[B](scope: F ~> F)(value: Kleisli[F, A, B]): Kleisli[F, A, B] =
        value.mapK(scope)
    }

  // TODO: move to an inner object
  implicit def stateT[F[_]: Functor, S]: LiftScope[F, StateT[F, S, *]] =
    new LiftScope[F, StateT[F, S, *]] {
      def liftScopeApply[A](scope: F ~> F)(value: StateT[F, S, A]): StateT[F, S, A] =
        value.mapK(scope)
    }

  implicit def writerT[F[_], L]: LiftScope[F, WriterT[F, L, *]] =
    new LiftScope[F, WriterT[F, L, *]] {
      def liftScopeApply[A](scope: F ~> F)(value: WriterT[F, L, A]): WriterT[F, L, A] =
        value.mapK(scope)
    }
}
