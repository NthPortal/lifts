package lgbt.princess.lifts

import cats.arrow.FunctionK
import cats.data.{EitherT, IorT, Kleisli, OptionT, StateT, WriterT}
import cats.{Applicative, Functor, Monoid, ~>}

import scala.annotation.implicitNotFound

/** Lifts values from the higher-kinded type `F` to the higher-kinded type `G`. */
@implicitNotFound("no way defined to lift values from ${F} to ${G}")
trait LiftValue[F[_], G[_]] {

  /**
   * Lifts `F[A]` into `G[A]`.
   *
   * @note
   *   This method is usually best implemented by a `liftF` method on `G`'s companion object.
   */
  def liftF[A](value: F[A]): G[A]

  /**
   * @return
   *   a higher-kinded function that lifts `F` into `G`.
   *
   * @note
   *   This method is usually best implemented by a `liftK` method on `G`'s companion object.
   */
  def liftK: F ~> G
}

object LiftValue extends LowPriorityLiftValueImplicits {

  /**
   * An instance that derives [[LiftValue.liftF `liftF`]] from [[LiftValue.liftK `liftK`]].
   */
  trait LiftFFromLiftK[F[_], G[_]] extends LiftValue[F, G] {
    val liftK: F ~> G
    final def liftF[A](value: F[A]): G[A] = liftK(value)
  }

  /**
   * An instance that derives [[LiftValue.liftK `liftK`]] from [[LiftValue.liftF `liftF`]].
   */
  trait LiftKFromLiftF[F[_], G[_]] extends LiftValue[F, G] {
    final def liftK: F ~> G =
      new (F ~> G) {
        def apply[A](fa: F[A]): G[A] = liftF(fa)
      }
  }

  /**
   * @return
   *   an instance that uses the given `liftK` function to lift values of the higher-kinded type `F`
   *   into the higher-kinded type `G`
   */
  def fromLiftK[F[_], G[_]](lift: F ~> G): LiftValue[F, G] =
    new LiftFFromLiftK[F, G] {
      val liftK: F ~> G = lift
    }

  implicit def id[F[_]]: LiftValue[F, F] =
    new LiftValue[F, F] {
      def liftF[A](value: F[A]): F[A] = value
      val liftK: F ~> F = FunctionK.id
    }

  implicit def optionT[F[_]: Functor]: LiftValue[F, OptionT[F, *]] =
    new LiftValue[F, OptionT[F, *]] {
      def liftF[A](value: F[A]): OptionT[F, A] = OptionT.liftF(value)
      val liftK: F ~> OptionT[F, *] = OptionT.liftK
    }

  implicit def eitherT[F[_]: Functor, L]: LiftValue[F, EitherT[F, L, *]] =
    new LiftValue[F, EitherT[F, L, *]] {
      def liftF[A](value: F[A]): EitherT[F, L, A] = EitherT.liftF(value)
      val liftK: F ~> EitherT[F, L, *] = EitherT.liftK
    }

  implicit def iorT[F[_]: Functor, L]: LiftValue[F, IorT[F, L, *]] =
    new LiftValue[F, IorT[F, L, *]] {
      def liftF[A](value: F[A]): IorT[F, L, A] = IorT.right(value)
      val liftK: F ~> IorT[F, L, *] = IorT.liftK
    }

  implicit def kleisli[F[_], A]: LiftValue[F, Kleisli[F, A, *]] =
    new LiftValue[F, Kleisli[F, A, *]] {
      def liftF[B](value: F[B]): Kleisli[F, A, B] = Kleisli.liftF(value)
      val liftK: F ~> Kleisli[F, A, *] = Kleisli.liftK
    }

  implicit def stateT[F[_]: Applicative, S]: LiftValue[F, StateT[F, S, *]] =
    new LiftValue[F, StateT[F, S, *]] {
      def liftF[A](value: F[A]): StateT[F, S, A] = StateT.liftF(value)
      val liftK: F ~> StateT[F, S, *] = StateT.liftK
    }

  implicit def writerT[F[_]: Applicative, L: Monoid]: LiftValue[F, WriterT[F, L, *]] =
    new LiftValue[F, WriterT[F, L, *]] {
      def liftF[A](value: F[A]): WriterT[F, L, A] = WriterT.liftF(value)
      val liftK: F ~> WriterT[F, L, *] = WriterT.liftK
    }
}

sealed trait LowPriorityLiftValueImplicits {
  implicit def liftValue1FromLiftKind2[F[_], G[_], H[_], I[_]](implicit
      lk2: LiftKind2[F, G, H, I]
  ): LiftValue[F, H] =
    lk2.liftValue1

  implicit def liftValue2FromLiftKind2[F[_], G[_], H[_], I[_]](implicit
      lk2: LiftKind2[F, G, H, I]
  ): LiftValue[G, I] =
    lk2.liftValue2
}
