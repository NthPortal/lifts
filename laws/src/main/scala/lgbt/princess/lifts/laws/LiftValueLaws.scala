package lgbt.princess.lifts
package laws

import cats.Functor
import cats.laws.IsEq
import cats.laws.IsEqArrow

trait LiftValueLaws[F[_], G[_]] {
  implicit def liftInstance: LiftValue[F, G]
  implicit def unliftInstance: Unlift[G, F]
  implicit final def functor: Functor[F] = unliftInstance.functor

  // internal laws:
  def liftFLiftKConsistency[A](fa: F[A]): IsEq[G[A]] =
    liftInstance.liftF(fa) <-> liftInstance.liftK(fa)

  def liftFIsReversible[A](fa: F[A]): IsEq[Unlift.Result[F, A]] =
    unliftInstance.unlift(liftInstance.liftF(fa)) <-> Unlift.success(fa)
}

object LiftValueLaws {
  def apply[F[_], G[_]](implicit
      lift: LiftValue[F, G],
      unlift: Unlift[G, F],
  ): LiftValueLaws[F, G] =
    new LiftValueLaws[F, G] {
      implicit val liftInstance: LiftValue[F, G] = lift
      implicit val unliftInstance: Unlift[G, F] = unlift
    }
}
