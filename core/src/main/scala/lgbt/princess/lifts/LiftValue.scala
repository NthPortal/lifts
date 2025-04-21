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

  /** @return an instance that lifts `F` to `G` and then `G` to `H` */
  def andThen[H[_]](that: LiftValue[G, H]): LiftValue[F, H] =
    LiftValue.Composed(this, that)

  /** @return an instance that lifts `E` to `F` and then `F` to `G` */
  def compose[E[_]](that: LiftValue[E, F]): LiftValue[E, G] =
    LiftValue.Composed(that, this)
}

object LiftValue extends LowPriorityLiftValueImplicits0 {
  private[this] final class Composed[F[_], G[_], H[_]] private (
      inner: LiftValue[F, G],
      outer: LiftValue[G, H]
  ) extends LiftValue[F, H] {
    def liftF[A](value: F[A]): H[A] = outer.liftF(inner.liftF(value))
    val liftK: F ~> H = inner.liftK.andThen(outer.liftK)
  }

  private object Composed {
    def apply[F[_], G[_], H[_]](inner: LiftValue[F, G], outer: LiftValue[G, H]): LiftValue[F, H] =
      if (inner.isInstanceOf[Identity]) outer.asInstanceOf[LiftValue[F, H]]
      else if (outer.isInstanceOf[Identity]) inner.asInstanceOf[LiftValue[F, H]]
      else new Composed(inner, outer)
  }

  def apply[F[_], G[_]](implicit lv: LiftValue[F, G]): LiftValue[F, G] = lv

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
    new LiftValue[F, F] with Identity {
      def liftF[A](value: F[A]): F[A] = value
      val liftK: F ~> F = FunctionK.id
    }

  implicit def optionT[F[_], G[_]: Functor](implicit
      inner: LiftValue[F, G]
  ): LiftValue[F, OptionT[G, *]] =
    inner.andThen {
      new LiftValue[G, OptionT[G, *]] {
        def liftF[A](value: G[A]): OptionT[G, A] = OptionT.liftF(value)
        val liftK: G ~> OptionT[G, *] = OptionT.liftK
      }
    }

  implicit def eitherT[F[_], G[_]: Functor, L](implicit
      inner: LiftValue[F, G]
  ): LiftValue[F, EitherT[G, L, *]] =
    inner.andThen {
      new LiftValue[G, EitherT[G, L, *]] {
        def liftF[A](value: G[A]): EitherT[G, L, A] = EitherT.liftF(value)
        val liftK: G ~> EitherT[G, L, *] = EitherT.liftK
      }
    }

  implicit def iorT[F[_], G[_]: Functor, L](implicit
      inner: LiftValue[F, G]
  ): LiftValue[F, IorT[G, L, *]] =
    inner.andThen {
      new LiftValue[G, IorT[G, L, *]] {
        def liftF[A](value: G[A]): IorT[G, L, A] = IorT.right(value)
        val liftK: G ~> IorT[G, L, *] = IorT.liftK
      }
    }

  implicit def kleisli[F[_], G[_], A](implicit
      inner: LiftValue[F, G]
  ): LiftValue[F, Kleisli[G, A, *]] =
    inner.andThen {
      new LiftValue[G, Kleisli[G, A, *]] {
        def liftF[B](value: G[B]): Kleisli[G, A, B] = Kleisli.liftF(value)
        val liftK: G ~> Kleisli[G, A, *] = Kleisli.liftK
      }
    }

  implicit def stateT[F[_], G[_]: Applicative, S](implicit
      inner: LiftValue[F, G]
  ): LiftValue[F, StateT[G, S, *]] =
    inner.andThen {
      new LiftValue[G, StateT[G, S, *]] {
        def liftF[A](value: G[A]): StateT[G, S, A] = StateT.liftF(value)
        val liftK: G ~> StateT[G, S, *] = StateT.liftK
      }
    }

  implicit def writerT[F[_], G[_]: Applicative, L: Monoid](implicit
      inner: LiftValue[F, G]
  ): LiftValue[F, WriterT[G, L, *]] =
    inner.andThen {
      new LiftValue[G, WriterT[G, L, *]] {
        def liftF[A](value: G[A]): WriterT[G, L, A] = WriterT.liftF(value)
        val liftK: G ~> WriterT[G, L, *] = WriterT.liftK
      }
    }
}

sealed trait LowPriorityLiftValueImplicits0 extends LowPriorityLiftValueImplicits1 {
  implicit def liftValue1FromLiftKind2[F[_], G[_], H[_], I[_]](implicit
      lk2: LiftKind2[F, G, H, I]
  ): LiftValue[F, H] =
    lk2.liftValue1
}

sealed trait LowPriorityLiftValueImplicits1 {
  implicit def liftValue2FromLiftKind2[F[_], G[_], H[_], I[_]](implicit
      lk2: LiftKind2[F, G, H, I]
  ): LiftValue[G, I] =
    lk2.liftValue2
}
