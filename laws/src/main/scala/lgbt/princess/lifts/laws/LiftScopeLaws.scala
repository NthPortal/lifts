package lgbt.princess.lifts
package laws

import cats.arrow.FunctionK
import cats.~>
import cats.laws.IsEq
import cats.laws.IsEqArrow

trait LiftScopeLaws[F[_], G[_]] {
  implicit def liftInstance: LiftScope[F, G]

  // external law:
  def liftScopeApplyIdentityIsPure[A](ga: G[A]): IsEq[G[A]] =
    liftInstance.liftScopeApply(FunctionK.id)(ga) <-> ga

  // internal law:
  def liftScopeApplyLiftScopeConsistency[A](scope: F ~> F, ga: G[A]): IsEq[G[A]] =
    liftInstance.liftScopeApply(scope)(ga) <-> liftInstance.liftScope(scope)(ga)
}

object LiftScopeLaws {
  def apply[F[_], G[_]](implicit lift: LiftScope[F, G]): LiftScopeLaws[F, G] =
    new LiftScopeLaws[F, G] {
      implicit val liftInstance: LiftScope[F, G] = lift
    }
}
