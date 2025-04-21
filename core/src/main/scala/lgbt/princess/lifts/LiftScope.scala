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

  def andThen[H[_]](that: LiftScope[G, H]): LiftScope[F, H] =
    LiftScope.Composed(this, that)

  def compose[E[_]](that: LiftScope[E, F]): LiftScope[E, G] =
    LiftScope.Composed(that, this)
}

object LiftScope {

  private[this] final class Composed[F[_], G[_], H[_]] private (
      inner: LiftScope[F, G],
      outer: LiftScope[G, H]
  ) extends LiftScope[F, H] {
    def liftScopeApply[A](scope: F ~> F)(value: H[A]): H[A] =
      outer.liftScopeApply(inner.liftScope(scope))(value)
    override def liftScope(scope: F ~> F): H ~> H =
      outer.liftScope(inner.liftScope(scope))
  }

  private object Composed {
    def apply[F[_], G[_], H[_]](inner: LiftScope[F, G], outer: LiftScope[G, H]): LiftScope[F, H] =
      if (inner.isInstanceOf[Identity]) outer.asInstanceOf[LiftScope[F, H]]
      else if (outer.isInstanceOf[Identity]) inner.asInstanceOf[LiftScope[F, H]]
      else new Composed(inner, outer)
  }

  def apply[F[_], G[_]](implicit ls: LiftScope[F, G]): LiftScope[F, G] = ls

  implicit def id[F[_]]: LiftScope[F, F] =
    new LiftScope[F, F] with Identity {
      def liftScopeApply[A](scope: F ~> F)(value: F[A]): F[A] = scope(value)
      override def liftScope(scope: F ~> F): F ~> F = scope
    }

  implicit def optionT[F[_], G[_]](implicit inner: LiftScope[F, G]): LiftScope[F, OptionT[G, *]] =
    inner.andThen {
      new LiftScope[G, OptionT[G, *]] {
        def liftScopeApply[A](scope: G ~> G)(value: OptionT[G, A]): OptionT[G, A] =
          value.mapK(scope)
      }
    }

  implicit def eitherT[F[_], G[_], L](implicit
      inner: LiftScope[F, G]
  ): LiftScope[F, EitherT[G, L, *]] =
    inner.andThen {
      new LiftScope[G, EitherT[G, L, *]] {
        def liftScopeApply[A](scope: G ~> G)(value: EitherT[G, L, A]): EitherT[G, L, A] =
          value.mapK(scope)
      }
    }

  implicit def iorT[F[_], G[_], L](implicit inner: LiftScope[F, G]): LiftScope[F, IorT[G, L, *]] =
    inner.andThen {
      new LiftScope[G, IorT[G, L, *]] {
        def liftScopeApply[A](scope: G ~> G)(value: IorT[G, L, A]): IorT[G, L, A] =
          value.mapK(scope)
      }
    }

  implicit def kleisli[F[_], G[_], A](implicit
      inner: LiftScope[F, G]
  ): LiftScope[F, Kleisli[G, A, *]] =
    inner.andThen {
      new LiftScope[G, Kleisli[G, A, *]] {
        def liftScopeApply[B](scope: G ~> G)(value: Kleisli[G, A, B]): Kleisli[G, A, B] =
          value.mapK(scope)
      }
    }

  // TODO: move to an inner object
  implicit def stateT[F[_], G[_]: Functor, S](implicit
      inner: LiftScope[F, G]
  ): LiftScope[F, StateT[G, S, *]] =
    inner.andThen {
      new LiftScope[G, StateT[G, S, *]] {
        def liftScopeApply[A](scope: G ~> G)(value: StateT[G, S, A]): StateT[G, S, A] =
          value.mapK(scope)
      }
    }

  implicit def writerT[F[_], G[_], L](implicit
      inner: LiftScope[F, G]
  ): LiftScope[F, WriterT[G, L, *]] =
    inner.andThen {
      new LiftScope[G, WriterT[G, L, *]] {
        def liftScopeApply[A](scope: G ~> G)(value: WriterT[G, L, A]): WriterT[G, L, A] =
          value.mapK(scope)
      }
    }
}
