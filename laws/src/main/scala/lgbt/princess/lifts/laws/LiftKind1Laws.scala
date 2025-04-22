package lgbt.princess.lifts
package laws

import cats.arrow.FunctionK
import cats.laws.IsEq
import cats.laws.IsEqArrow
import cats.~>

trait LiftKind1Laws[F[_], G[_]] extends LiftValueLaws[F, G] with MapKLaws[F, F, G, G] {
  implicit def liftInstance: LiftKind1[F, G]
  implicit final def mapKInstance: MapK[F, F, G, G] = liftInstance

  // external law:
  def mapKIdentityIsPure[A](ga: G[A]): IsEq[G[A]] =
    liftInstance.mapK(ga)(FunctionK.id) <-> ga

  // internal laws:
  def liftFMapKConsistency[A](fa: F[A], scope: F ~> F): IsEq[G[A]] =
    liftInstance.liftF(scope(fa)) <->
      liftInstance.mapK(liftInstance.liftF(fa))(scope)

  def mapKIsReversible[A](fa: F[A], scope: F ~> F): IsEq[Unlift.Result[F, A]] =
    unliftInstance.unlift(liftInstance.mapK(liftInstance.liftF(fa))(scope)) <->
      Unlift.success(scope(fa))
}

object LiftKind1Laws {
  def apply[F[_], G[_]](implicit lift: LiftKind1[F, G], unlift: Unlift[G, F]): LiftKind1Laws[F, G] =
    new LiftKind1Laws[F, G] {
      implicit val liftInstance: LiftKind1[F, G] = lift
      implicit val unliftInstance: Unlift[G, F] = unlift
      implicit def unliftHFInstance: Unlift[G, F] = unliftInstance
      implicit def unliftIGInstance: Unlift[G, F] = unliftInstance
    }
}
