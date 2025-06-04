package lgbt.princess.lifts

import cats.arrow.FunctionK
import cats.data.{EitherT, IorT, Kleisli, OptionT, WriterT}
import cats.{Applicative, Functor, Monoid, ~>}

trait LiftKind1[From[_], To[_]] extends LiftValue[From, To] with LiftScopeAlt[From, To] {
  final def liftScope(scope: From ~> From): To ~> To = liftFunctionK(scope)

  def andThen[Last[_]](that: LiftKind1[To, Last]): LiftKind1[From, Last] =
    LiftKind1.Composed(this, that)

  def compose[First[_]](that: LiftKind1[First, From]): LiftKind1[First, To] =
    LiftKind1.Composed(that, this)
}

object LiftKind1 extends LowPriorityLiftKind1Implicits {

  private[this] final class Composed[First[_], Middle[_], Last[_]] private (
      inner: LiftKind1[First, Middle],
      outer: LiftKind1[Middle, Last],
  ) extends LiftKind1[First, Last] {
    def liftF[A](value: First[A]): Last[A] = outer.liftF(inner.liftF(value))
    val liftK: First ~> Last = inner.liftK.andThen(outer.liftK)
    def mapK[A](value: Last[A])(f: First ~> First): Last[A] =
      outer.mapK(value)(inner.liftFunctionK(f))
    override def liftFunctionK(f: First ~> First): Last ~> Last =
      outer.liftFunctionK(inner.liftFunctionK(f))
  }

  private object Composed {
    def apply[From[_], Middle[_], To[_]](
        inner: LiftKind1[From, Middle],
        outer: LiftKind1[Middle, To],
    ): LiftKind1[From, To] =
      if (inner.isInstanceOf[Identity]) outer.asInstanceOf[LiftKind1[From, To]]
      else if (outer.isInstanceOf[Identity]) inner.asInstanceOf[LiftKind1[From, To]]
      else new Composed(inner, outer)
  }

  type Derived[From[_], Wrapper[_[_], _]] = LiftKind1[From, Wrapper[From, *]]

  def apply[From[_], To[_]](implicit ev: LiftKind1[From, To]): LiftKind1[From, To] = ev

  implicit def id[F[_]]: LiftKind1[F, F] =
    new LiftKind1[F, F] with Identity {
      def liftF[A](value: F[A]): F[A] = value
      def liftK: F ~> F = FunctionK.id
      def mapK[A](value: F[A])(scope: F ~> F): F[A] = scope(value)
      override def liftFunctionK(scope: F ~> F): F ~> F = scope
    }

  implicit def optionT[From[_], To[_]: Functor](implicit
      inner: LiftKind1[From, To]
  ): LiftKind1[From, OptionT[To, *]] =
    inner.andThen {
      new LiftKind1[To, OptionT[To, *]] {
        def liftF[A](value: To[A]): OptionT[To, A] = OptionT.liftF(value)
        val liftK: To ~> OptionT[To, *] = OptionT.liftK
        def mapK[A](value: OptionT[To, A])(scope: To ~> To): OptionT[To, A] =
          value.mapK(scope)
      }
    }

  implicit def eitherT[From[_], To[_]: Functor, L](implicit
      inner: LiftKind1[From, To]
  ): LiftKind1[From, EitherT[To, L, *]] =
    inner.andThen {
      new LiftKind1[To, EitherT[To, L, *]] {
        def liftF[A](value: To[A]): EitherT[To, L, A] = EitherT.liftF(value)
        val liftK: To ~> EitherT[To, L, *] = EitherT.liftK
        def mapK[A](value: EitherT[To, L, A])(scope: To ~> To): EitherT[To, L, A] =
          value.mapK(scope)
      }
    }

  implicit def iorT[From[_], To[_]: Functor, L](implicit
      inner: LiftKind1[From, To]
  ): LiftKind1[From, IorT[To, L, *]] =
    inner.andThen {
      new LiftKind1[To, IorT[To, L, *]] {
        def liftF[A](value: To[A]): IorT[To, L, A] = IorT.right(value)
        val liftK: To ~> IorT[To, L, *] = IorT.liftK
        def mapK[A](value: IorT[To, L, A])(scope: To ~> To): IorT[To, L, A] =
          value.mapK(scope)
      }
    }

  implicit def kleisli[From[_], To[_], A](implicit
      inner: LiftKind1[From, To]
  ): LiftKind1[From, Kleisli[To, A, *]] =
    inner.andThen {
      new LiftKind1[To, Kleisli[To, A, *]] {
        def liftF[B](value: To[B]): Kleisli[To, A, B] = Kleisli.liftF(value)
        val liftK: To ~> Kleisli[To, A, *] = Kleisli.liftK
        def mapK[B](value: Kleisli[To, A, B])(scope: To ~> To): Kleisli[To, A, B] =
          value.mapK(scope)
      }
    }

  implicit def writerT[From[_], To[_]: Applicative, L: Monoid](implicit
      inner: LiftKind1[From, To]
  ): LiftKind1[From, WriterT[To, L, *]] =
    inner.andThen {
      new LiftKind1[To, WriterT[To, L, *]] {
        def liftF[A](value: To[A]): WriterT[To, L, A] = WriterT.liftF(value)
        val liftK: To ~> WriterT[To, L, *] = WriterT.liftK
        def mapK[A](value: WriterT[To, L, A])(scope: To ~> To): WriterT[To, L, A] =
          value.mapK(scope)
      }
    }
}

trait LowPriorityLiftKind1Implicits {
  implicit def liftKind1FromLiftKind2[From[_], To[_]](implicit
      lk2: LiftKind2[From, From, To, To]
  ): LiftKind1[From, To] =
    new LiftKind1[From, To] {
      def liftF[A](value: From[A]): To[A] = lk2.liftValueInput.liftF(value)
      def liftK: From ~> To = lk2.liftValueInput.liftK
      def mapK[A](value: To[A])(f: From ~> From): To[A] = lk2.mapK(value)(f)
      override def liftFunctionK(f: From ~> From): To ~> To = lk2.liftFunctionK(f)
    }
}
