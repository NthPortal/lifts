package lgbt.princess.lifts
package laws

import cats.laws.IsEq
import cats.laws.IsEqArrow
import cats.~>

trait LiftKindLaws[From[_], To[_]] extends LiftValueLaws[From, To] with LiftScopeLaws[From, To] {
  implicit def liftInstance: LiftKind[From, To]

  // internal laws:
  def liftFLimitedMapKConsistency[A](fa: From[A], scope: From ~> From): IsEq[To[A]] =
    liftInstance.liftF(scope(fa)) <->
      liftInstance.limitedMapK(liftInstance.liftF(fa))(scope)

  def limitedMapKIsReversible[A](fa: From[A], scope: From ~> From): IsEq[Unlift.Result[From, A]] =
    unliftInstance.unlift(liftInstance.limitedMapK(liftInstance.liftF(fa))(scope)) <->
      Unlift.success(scope(fa))
}

object LiftKindLaws {
  def apply[From[_], To[_]](implicit
      lift: LiftKind[From, To],
      unlift: Unlift[To, From],
  ): LiftKindLaws[From, To] =
    new LiftKindLaws[From, To] {
      implicit val liftInstance: LiftKind[From, To] = lift
      implicit val unliftInstance: Unlift[To, From] = unlift
    }
}
