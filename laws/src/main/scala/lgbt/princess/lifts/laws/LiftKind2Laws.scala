package lgbt.princess.lifts
package laws

import cats.laws.IsEq
import cats.laws.IsEqArrow
import cats.{Functor, ~>}

trait LiftKind2Laws[In1[_], Out1[_], In2[_], Out2[_]] extends MapKLaws[In1, Out1, In2, Out2] {
  implicit def liftInstance: LiftKind2[In1, Out1, In2, Out2]
  implicit final def mapKInstance: MapK[In1, Out1, In2, Out2] = liftInstance
  implicit def functorOut1: Functor[Out1] = unliftOutInstance.functor

  // internal laws:
  def liftFMapKConsistency[A](fa: In1[A], f: In1 ~> Out1): IsEq[Out2[A]] =
    liftInstance.liftValueOutput.liftF(f(fa)) <->
      liftInstance.mapK(liftInstance.liftValueInput.liftF(fa))(f)

  def mapKIsReversible[A](fa: In1[A], f: In1 ~> Out1): IsEq[Unlift.Result[Out1, A]] =
    unliftOutInstance.unlift(liftInstance.mapK(liftInstance.liftValueInput.liftF(fa))(f)) <->
      Unlift.success(f(fa))
}

object LiftKind2Laws {
  def apply[In1[_], Out1[_], In2[_], Out2[_]](implicit
      lift: LiftKind2[In1, Out1, In2, Out2],
      unliftIn: Unlift[In2, In1],
      unliftOut: Unlift[Out2, Out1],
  ): LiftKind2Laws[In1, Out1, In2, Out2] =
    new LiftKind2Laws[In1, Out1, In2, Out2] {
      implicit val liftInstance: LiftKind2[In1, Out1, In2, Out2] = lift
      implicit val unliftInInstance: Unlift[In2, In1] = unliftIn
      implicit val unliftOutInstance: Unlift[Out2, Out1] = unliftOut
    }
}
