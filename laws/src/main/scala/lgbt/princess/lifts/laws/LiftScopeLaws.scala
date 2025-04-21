package lgbt.princess.lifts
package laws

import cats.arrow.FunctionK
import cats.~>
import cats.laws.IsEq
import cats.laws.IsEqArrow

trait LiftScopeLaws[F[_], G[_]] {
  implicit def liftInstance: LiftScope[F, G]

  // external law:
  def limitedMapKIdentityIsPure[A](ga: G[A]): IsEq[G[A]] =
    liftInstance.limitedMapK(ga)(FunctionK.id) <-> ga

  // internal law:
  def limitedMapKLiftScopeConsistency[A](scope: F ~> F, ga: G[A]): IsEq[G[A]] =
    liftInstance.limitedMapK(ga)(scope) <-> liftInstance.liftScope(scope)(ga)
}

object LiftScopeLaws {
  def apply[F[_], G[_]](implicit lift: LiftScope[F, G]): LiftScopeLaws[F, G] =
    new LiftScopeLaws[F, G] {
      implicit val liftInstance: LiftScope[F, G] = lift
    }
}
