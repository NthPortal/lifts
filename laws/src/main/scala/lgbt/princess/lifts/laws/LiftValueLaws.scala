package lgbt.princess.lifts
package laws

import cats.Functor
import cats.laws.IsEq
import cats.laws.IsEqArrow

trait LiftValueLaws[From[_], To[_]] {
  implicit def liftInstance: LiftValue[From, To]
  implicit def unliftInstance: Unlift[To, From]
  implicit final def functor: Functor[From] = unliftInstance.functor

  // internal laws:
  def liftFLiftKConsistency[A](fa: From[A]): IsEq[To[A]] =
    liftInstance.liftF(fa) <-> liftInstance.liftK(fa)

  def liftFIsReversible[A](fa: From[A]): IsEq[Unlift.Result[From, A]] =
    unliftInstance.unlift(liftInstance.liftF(fa)) <-> Unlift.success(fa)
}

object LiftValueLaws {
  def apply[From[_], To[_]](implicit
      lift: LiftValue[From, To],
      unlift: Unlift[To, From],
  ): LiftValueLaws[From, To] =
    new LiftValueLaws[From, To] {
      implicit val liftInstance: LiftValue[From, To] = lift
      implicit val unliftInstance: Unlift[To, From] = unlift
    }
}
