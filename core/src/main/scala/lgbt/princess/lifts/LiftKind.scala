package lgbt.princess.lifts

import cats.data.{EitherT, IorT, Kleisli, OptionT, WriterT}
import cats.{Applicative, Functor, Monoid, ~>}

import scala.annotation.implicitNotFound

/**
 * Lifts values and scope transformations from the higher-kinded type `From` to the higher-kinded
 * type `To`.
 */
@implicitNotFound("no way defined to lift values and scopes from ${From} to ${To}")
trait LiftKind[From[_], To[_]] extends LiftValue[From, To] with LiftScope[From, To] {
  def andThen[Last[_]](that: LiftKind[To, Last]): LiftKind[From, Last] =
    LiftKind.Composed(this, that)

  def compose[First[_]](that: LiftKind[First, From]): LiftKind[First, To] =
    LiftKind.Composed(that, this)
}

object LiftKind {

  type Derived[From[_], Wrapper[_[_], _]] = LiftKind[From, Wrapper[From, *]]

  def apply[From[_], To[_]](implicit ev: LiftKind[From, To]): LiftKind[From, To] = ev

  private[this] final class Identity[F[_]]
      extends LiftKind[F, F]
      with LiftValue.Identity[F]
      with LiftScope.Identity[F]

  private[this] val _identity = new Identity[({ type L[_] = Any })#L]

  private[this] final class Composed[From[_], Middle[_], To[_]] private (
      inner: LiftKind[From, Middle],
      outer: LiftKind[Middle, To],
  ) extends LiftKind[From, To] {
    def liftF[A](value: From[A]): To[A] = outer.liftF(inner.liftF(value))
    def limitedMapK[A](value: To[A])(scope: From ~> From): To[A] =
      outer.limitedMapK(value)(inner.liftScope(scope))
    override def liftScope(scope: From ~> From): To ~> To =
      outer.liftScope(inner.liftScope(scope))
  }

  private object Composed {
    def apply[From[_], Middle[_], To[_]](
        inner: LiftKind[From, Middle],
        outer: LiftKind[Middle, To],
    ): LiftKind[From, To] =
      if (inner.isInstanceOf[Identity[From]]) outer.asInstanceOf[LiftKind[From, To]]
      else if (outer.isInstanceOf[Identity[Middle]]) inner.asInstanceOf[LiftKind[From, To]]
      else new Composed(inner, outer)
  }

  implicit def id[F[_]]: LiftKind[F, F] = _identity.asInstanceOf[Identity[F]]

  implicit def eitherT[From[_], To[_]: Functor, L](implicit
      inner: LiftKind[From, To]
  ): LiftKind[From, EitherT[To, L, *]] =
    inner.andThen {
      new LiftKind[To, EitherT[To, L, *]] {
        def liftF[A](value: To[A]): EitherT[To, L, A] = EitherT.liftF(value)
        def limitedMapK[A](value: EitherT[To, L, A])(scope: To ~> To): EitherT[To, L, A] =
          value.mapK(scope)
      }
    }

  implicit def iorT[From[_], To[_]: Functor, L](implicit
      inner: LiftKind[From, To]
  ): LiftKind[From, IorT[To, L, *]] =
    inner.andThen {
      new LiftKind[To, IorT[To, L, *]] {
        def liftF[A](value: To[A]): IorT[To, L, A] = IorT.right(value)
        def limitedMapK[A](value: IorT[To, L, A])(scope: To ~> To): IorT[To, L, A] =
          value.mapK(scope)
      }
    }

  implicit def kleisli[From[_], To[_], A](implicit
      inner: LiftKind[From, To]
  ): LiftKind[From, Kleisli[To, A, *]] =
    inner.andThen {
      new LiftKind[To, Kleisli[To, A, *]] {
        def liftF[B](value: To[B]): Kleisli[To, A, B] = Kleisli.liftF(value)
        def limitedMapK[B](value: Kleisli[To, A, B])(scope: To ~> To): Kleisli[To, A, B] =
          value.mapK(scope)
      }
    }

  implicit def optionT[From[_], To[_]: Functor](implicit
      inner: LiftKind[From, To]
  ): LiftKind[From, OptionT[To, *]] =
    inner.andThen {
      new LiftKind[To, OptionT[To, *]] {
        def liftF[A](value: To[A]): OptionT[To, A] = OptionT.liftF(value)
        def limitedMapK[A](value: OptionT[To, A])(scope: To ~> To): OptionT[To, A] =
          value.mapK(scope)
      }
    }

  implicit def writerT[From[_], To[_]: Applicative, L: Monoid](implicit
      inner: LiftKind[From, To]
  ): LiftKind[From, WriterT[To, L, *]] =
    inner.andThen {
      new LiftKind[To, WriterT[To, L, *]] {
        def liftF[A](value: To[A]): WriterT[To, L, A] = WriterT.liftF(value)
        def limitedMapK[A](value: WriterT[To, L, A])(scope: To ~> To): WriterT[To, L, A] =
          value.mapK(scope)
      }
    }
}
