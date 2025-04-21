package lgbt.princess.lifts
package laws

import cats.laws.IsEq
import cats.laws.IsEqArrow
import cats.~>

trait LiftKindLaws[F[_], G[_]] extends LiftValueLaws[F, G] with LiftScopeLaws[F, G] {
  implicit def liftInstance: LiftKind[F, G]

  // internal laws:
  def liftFLimitedMapKConsistency[A](scope: F ~> F, fa: F[A]): IsEq[G[A]] =
    liftInstance.liftF(scope(fa)) <->
      liftInstance.limitedMapK(liftInstance.liftF(fa))(scope)

  def limitedMapKIsReversible[A](scope: F ~> F, fa: F[A]): IsEq[Unlift.Result[F, A]] =
    unliftInstance.unlift(liftInstance.limitedMapK(liftInstance.liftF(fa))(scope)) <->
      Unlift.success(scope(fa))
}

object LiftKindLaws {
  def apply[F[_], G[_]](implicit
      lift: LiftKind[F, G],
      unlift: Unlift[G, F],
  ): LiftKindLaws[F, G] =
    new LiftKindLaws[F, G] {
      implicit val liftInstance: LiftKind[F, G] = lift
      implicit val unliftInstance: Unlift[G, F] = unlift
    }
}
