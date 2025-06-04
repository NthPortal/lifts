package lgbt.princess.lifts
package laws

import cats.laws.{IsEq, IsEqArrow}
import cats.~>

trait MapKLaws[In1[_], Out1[_], In2[_], Out2[_]] {
  implicit def mapKInstance: MapK[In1, Out1, In2, Out2]
  implicit def unliftInInstance: Unlift[In2, In1]
  implicit def unliftOutInstance: Unlift[Out2, Out1]

  // internal law:
  def mapKLiftFunctionKConsistency[A](ha: In2[A], f: In1 ~> Out1): IsEq[Out2[A]] =
    mapKInstance.mapK(ha)(f) <-> mapKInstance.liftFunctionK(f)(ha)

  def mapKUnliftConsistency[A](ha: In2[A], f: In1 ~> Out1): IsEq[Unlift.Result[Out1, A]] =
    unliftOutInstance.unlift(mapKInstance.mapK(ha)(f)) <->
      unliftInInstance.unlift(ha).mapK(f)
}

object MapKLaws {
  def apply[In1[_], Out1[_], In2[_], Out2[_]](implicit
      mk: MapK[In1, Out1, In2, Out2],
      unliftIn: Unlift[In2, In1],
      unliftOut: Unlift[Out2, Out1],
  ): MapKLaws[In1, Out1, In2, Out2] =
    new MapKLaws[In1, Out1, In2, Out2] {
      implicit val mapKInstance: MapK[In1, Out1, In2, Out2] = mk
      implicit val unliftInInstance: Unlift[In2, In1] = unliftIn
      implicit val unliftOutInstance: Unlift[Out2, Out1] = unliftOut
    }
}
