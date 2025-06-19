package lgbt.princess.lifts

import cats.data.{EitherT, IorT, Kleisli, OptionT, WriterT}
import cats.~>

import scala.annotation.implicitNotFound

/**
 * Lifts scope transformations from the higher-kinded type `From` to the higher-kinded type `To`.
 */
@implicitNotFound("no way defined to lift scopes from ${From} to ${To}")
trait LiftScope[From[_], To[_]] {

  /**
   * Modifies the context of `To[A]` using the given scope transformation in `From`.
   *
   * @note
   *   This method is usually best implemented by a `mapK` method on `To`.
   */
  def limitedMapK[A](value: To[A])(scope: From ~> From): To[A]

  /**
   * Lifts a scope transformation in `From` into a scope operation in `To`.
   *
   * @note
   *   Implementors SHOULD NOT override this method; the only reason it is not final is for
   *   optimization of the identity case.
   */
  def liftScope(scope: From ~> From): To ~> To =
    new (To ~> To) {
      def apply[A](fa: To[A]): To[A] = limitedMapK(fa)(scope)
    }

  def andThen[Last[_]](that: LiftScope[To, Last]): LiftScope[From, Last] =
    LiftScope.Composed(this, that)

  def compose[First[_]](that: LiftScope[First, From]): LiftScope[First, To] =
    LiftScope.Composed(that, this)
}

object LiftScope {

  type Derived[From[_], Wrapper[_[_], _]] = LiftScope[From, Wrapper[From, *]]

  def apply[From[_], To[_]](implicit ev: LiftScope[From, To]): LiftScope[From, To] = ev

  private[lifts] trait Identity[F[_]] extends LiftScope[F, F] {
    def limitedMapK[A](value: F[A])(scope: F ~> F): F[A] = scope(value)
    override def liftScope(scope: F ~> F): F ~> F = scope
  }

  private[this] val _identity = new Identity[({ type L[_] = Any })#L] {}

  private[this] final class Composed[From[_], Middle[_], To[_]] private (
      inner: LiftScope[From, Middle],
      outer: LiftScope[Middle, To],
  ) extends LiftScope[From, To] {
    def limitedMapK[A](value: To[A])(scope: From ~> From): To[A] =
      outer.limitedMapK(value)(inner.liftScope(scope))
    override def liftScope(scope: From ~> From): To ~> To =
      outer.liftScope(inner.liftScope(scope))
  }

  private object Composed {
    def apply[From[_], Middle[_], To[_]](
        inner: LiftScope[From, Middle],
        outer: LiftScope[Middle, To],
    ): LiftScope[From, To] =
      if (inner.isInstanceOf[Identity[From]]) outer.asInstanceOf[LiftScope[From, To]]
      else if (outer.isInstanceOf[Identity[Middle]]) inner.asInstanceOf[LiftScope[From, To]]
      else new Composed(inner, outer)
  }

  implicit def id[F[_]]: LiftScope[F, F] = _identity.asInstanceOf[Identity[F]]

  implicit def eitherT[From[_], To[_], L](implicit
      inner: LiftScope[From, To]
  ): LiftScope[From, EitherT[To, L, *]] =
    inner.andThen {
      new LiftScope[To, EitherT[To, L, *]] {
        def limitedMapK[A](value: EitherT[To, L, A])(scope: To ~> To): EitherT[To, L, A] =
          value.mapK(scope)
      }
    }

  implicit def iorT[From[_], To[_], L](implicit
      inner: LiftScope[From, To]
  ): LiftScope[From, IorT[To, L, *]] =
    inner.andThen {
      new LiftScope[To, IorT[To, L, *]] {
        def limitedMapK[A](value: IorT[To, L, A])(scope: To ~> To): IorT[To, L, A] =
          value.mapK(scope)
      }
    }

  implicit def kleisli[From[_], To[_], A](implicit
      inner: LiftScope[From, To]
  ): LiftScope[From, Kleisli[To, A, *]] =
    inner.andThen {
      new LiftScope[To, Kleisli[To, A, *]] {
        def limitedMapK[B](value: Kleisli[To, A, B])(scope: To ~> To): Kleisli[To, A, B] =
          value.mapK(scope)
      }
    }

  implicit def optionT[From[_], To[_]](implicit
      inner: LiftScope[From, To]
  ): LiftScope[From, OptionT[To, *]] =
    inner.andThen {
      new LiftScope[To, OptionT[To, *]] {
        def limitedMapK[A](value: OptionT[To, A])(scope: To ~> To): OptionT[To, A] =
          value.mapK(scope)
      }
    }

  implicit def writerT[From[_], To[_], L](implicit
      inner: LiftScope[From, To]
  ): LiftScope[From, WriterT[To, L, *]] =
    inner.andThen {
      new LiftScope[To, WriterT[To, L, *]] {
        def limitedMapK[A](value: WriterT[To, L, A])(scope: To ~> To): WriterT[To, L, A] =
          value.mapK(scope)
      }
    }
}
