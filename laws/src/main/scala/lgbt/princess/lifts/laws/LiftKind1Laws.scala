package lgbt.princess.lifts
package laws

import cats.arrow.FunctionK
import cats.laws.IsEq
import cats.laws.IsEqArrow
import cats.~>

trait LiftKind1Laws[From[_], To[_]]
    extends LiftValueLaws[From, To]
    with MapKLaws[From, From, To, To] {
  implicit def liftInstance: LiftKind1[From, To]
  implicit final def mapKInstance: MapK[From, From, To, To] = liftInstance

  // external law:
  def mapKIdentityIsPure[A](ga: To[A]): IsEq[To[A]] =
    liftInstance.mapK(ga)(FunctionK.id) <-> ga

  // internal laws:
  def liftFMapKConsistency[A](fa: From[A], scope: From ~> From): IsEq[To[A]] =
    liftInstance.liftF(scope(fa)) <->
      liftInstance.mapK(liftInstance.liftF(fa))(scope)

  def mapKIsReversible[A](fa: From[A], scope: From ~> From): IsEq[Unlift.Result[From, A]] =
    unliftInstance.unlift(liftInstance.mapK(liftInstance.liftF(fa))(scope)) <->
      Unlift.success(scope(fa))
}

object LiftKind1Laws {
  def apply[From[_], To[_]](implicit
      lift: LiftKind1[From, To],
      unlift: Unlift[To, From],
  ): LiftKind1Laws[From, To] =
    new LiftKind1Laws[From, To] {
      implicit val liftInstance: LiftKind1[From, To] = lift
      implicit val unliftInstance: Unlift[To, From] = unlift
      implicit def unliftInInstance: Unlift[To, From] = unliftInstance
      implicit def unliftOutInstance: Unlift[To, From] = unliftInstance
    }
}
