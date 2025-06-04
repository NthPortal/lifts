package lgbt.princess.lifts

import cats.arrow.FunctionK
import cats.data.{EitherT, IorT, Kleisli, OptionT, RWST, StateT, WriterT}
import cats.{Applicative, Functor, Monoid, ~>}

import scala.annotation.implicitNotFound

/** Lifts values from the higher-kinded type `From` to the higher-kinded type `To`. */
@implicitNotFound("no way defined to lift values from ${From} to ${To}")
trait LiftValue[From[_], To[_]] {

  /**
   * Lifts `From[A]` into `To[A]`.
   *
   * @note
   *   This method is usually best implemented by a `liftF` method on `To`'s companion object.
   */
  def liftF[A](value: From[A]): To[A]

  /**
   * @return
   *   a higher-kinded function that lifts `From` into `To`.
   *
   * @note
   *   This method is usually best implemented by a `liftK` method on `To`'s companion object.
   */
  def liftK: From ~> To

  /** @return an instance that lifts `From` to `To` and then `To` to `Last` */
  def andThen[Last[_]](that: LiftValue[To, Last]): LiftValue[From, Last] =
    LiftValue.Composed(this, that)

  /** @return an instance that lifts `First` to `From` and then `From` to `To` */
  def compose[First[_]](that: LiftValue[First, From]): LiftValue[First, To] =
    LiftValue.Composed(that, this)
}

object LiftValue extends LowPriorityLiftValueImplicits0 {

  private[this] final class Composed[From[_], Middle[_], To[_]] private (
      inner: LiftValue[From, Middle],
      outer: LiftValue[Middle, To]
  ) extends LiftValue[From, To] {
    def liftF[A](value: From[A]): To[A] = outer.liftF(inner.liftF(value))
    val liftK: From ~> To = inner.liftK.andThen(outer.liftK)
  }

  private object Composed {
    def apply[From[_], Middle[_], To[_]](
        inner: LiftValue[From, Middle],
        outer: LiftValue[Middle, To]
    ): LiftValue[From, To] =
      if (inner.isInstanceOf[Identity]) outer.asInstanceOf[LiftValue[From, To]]
      else if (outer.isInstanceOf[Identity]) inner.asInstanceOf[LiftValue[From, To]]
      else new Composed(inner, outer)
  }

  type Derived[From[_], Wrapper[_[_], _]] = LiftValue[From, Wrapper[From, *]]

  def apply[From[_], To[_]](implicit lv: LiftValue[From, To]): LiftValue[From, To] = lv

  /**
   * An instance that derives [[LiftValue.liftF `liftF`]] from [[LiftValue.liftK `liftK`]].
   */
  trait LiftFFromLiftK[From[_], To[_]] extends LiftValue[From, To] {
    val liftK: From ~> To
    final def liftF[A](value: From[A]): To[A] = liftK(value)
  }

  /**
   * An instance that derives [[LiftValue.liftK `liftK`]] from [[LiftValue.liftF `liftF`]].
   */
  trait LiftKFromLiftF[From[_], To[_]] extends LiftValue[From, To] {
    final def liftK: From ~> To =
      new (From ~> To) {
        def apply[A](fa: From[A]): To[A] = liftF(fa)
      }
  }

  /**
   * @return
   *   an instance that uses the given `liftK` function to lift values of the higher-kinded type
   *   `From` into the higher-kinded type `To`
   */
  def fromLiftK[From[_], To[_]](lift: From ~> To): LiftValue[From, To] =
    new LiftFFromLiftK[From, To] {
      val liftK: From ~> To = lift
    }

  implicit def id[F[_]]: LiftValue[F, F] =
    new LiftValue[F, F] with Identity {
      def liftF[A](value: F[A]): F[A] = value
      val liftK: F ~> F = FunctionK.id
    }

  implicit def optionT[From[_], To[_]: Functor](implicit
      inner: LiftValue[From, To]
  ): LiftValue[From, OptionT[To, *]] =
    inner.andThen {
      new LiftValue[To, OptionT[To, *]] {
        def liftF[A](value: To[A]): OptionT[To, A] = OptionT.liftF(value)
        val liftK: To ~> OptionT[To, *] = OptionT.liftK
      }
    }

  implicit def eitherT[From[_], To[_]: Functor, L](implicit
      inner: LiftValue[From, To]
  ): LiftValue[From, EitherT[To, L, *]] =
    inner.andThen {
      new LiftValue[To, EitherT[To, L, *]] {
        def liftF[A](value: To[A]): EitherT[To, L, A] = EitherT.liftF(value)
        val liftK: To ~> EitherT[To, L, *] = EitherT.liftK
      }
    }

  implicit def iorT[From[_], To[_]: Functor, L](implicit
      inner: LiftValue[From, To]
  ): LiftValue[From, IorT[To, L, *]] =
    inner.andThen {
      new LiftValue[To, IorT[To, L, *]] {
        def liftF[A](value: To[A]): IorT[To, L, A] = IorT.right(value)
        val liftK: To ~> IorT[To, L, *] = IorT.liftK
      }
    }

  implicit def kleisli[From[_], To[_], A](implicit
      inner: LiftValue[From, To]
  ): LiftValue[From, Kleisli[To, A, *]] =
    inner.andThen {
      new LiftValue[To, Kleisli[To, A, *]] {
        def liftF[B](value: To[B]): Kleisli[To, A, B] = Kleisli.liftF(value)
        val liftK: To ~> Kleisli[To, A, *] = Kleisli.liftK
      }
    }

  implicit def writerT[From[_], To[_]: Applicative, L: Monoid](implicit
      inner: LiftValue[From, To]
  ): LiftValue[From, WriterT[To, L, *]] =
    inner.andThen {
      new LiftValue[To, WriterT[To, L, *]] {
        def liftF[A](value: To[A]): WriterT[To, L, A] = WriterT.liftF(value)
        val liftK: To ~> WriterT[To, L, *] = WriterT.liftK
      }
    }

  implicit def stateT[From[_], To[_]: Applicative, S](implicit
      inner: LiftValue[From, To]
  ): LiftValue[From, StateT[To, S, *]] =
    inner.andThen {
      new LiftValue[To, StateT[To, S, *]] {
        def liftF[A](value: To[A]): StateT[To, S, A] = StateT.liftF(value)
        val liftK: To ~> StateT[To, S, *] = StateT.liftK
      }
    }

  implicit def rwst[From[_], To[_]: Applicative, E, L: Monoid, S](implicit
      inner: LiftValue[From, To]
  ): LiftValue[From, RWST[To, E, L, S, *]] =
    inner.andThen {
      new LiftValue[To, RWST[To, E, L, S, *]] {
        def liftF[A](value: To[A]): RWST[To, E, L, S, A] = RWST.liftF(value)
        val liftK: To ~> RWST[To, E, L, S, *] = RWST.liftK
      }
    }
}

sealed trait LowPriorityLiftValueImplicits0 extends LowPriorityLiftValueImplicits1 {
  implicit def liftValueInputFromLiftKind2[F[_], G[_], H[_], I[_]](implicit
                                                                   lk2: LiftKind2[F, G, H, I]
  ): LiftValue[F, H] =
    lk2.liftValueInput
}

sealed trait LowPriorityLiftValueImplicits1 {
  implicit def liftValueOutputFromLiftKind2[F[_], G[_], H[_], I[_]](implicit
                                                                    lk2: LiftKind2[F, G, H, I]
  ): LiftValue[G, I] =
    lk2.liftValueOutput
}
