package lgbt.princess.lifts
package laws

import cats.arrow.FunctionK
import cats.~>
import cats.laws.IsEq
import cats.laws.IsEqArrow

trait LiftScopeLaws[From[_], To[_]] {
  implicit def liftInstance: LiftScope[From, To]

  // external law:
  def limitedMapKIdentityIsPure[A](ga: To[A]): IsEq[To[A]] =
    liftInstance.limitedMapK(ga)(FunctionK.id) <-> ga

  // internal law:
  def limitedMapKLiftScopeConsistency[A](ga: To[A], scope: From ~> From): IsEq[To[A]] =
    liftInstance.limitedMapK(ga)(scope) <-> liftInstance.liftScope(scope)(ga)
}

object LiftScopeLaws {
  def apply[From[_], To[_]](implicit lift: LiftScope[From, To]): LiftScopeLaws[From, To] =
    new LiftScopeLaws[From, To] {
      implicit val liftInstance: LiftScope[From, To] = lift
    }
}
