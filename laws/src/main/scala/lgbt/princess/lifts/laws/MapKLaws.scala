package lgbt.princess.lifts
package laws

import cats.laws.{IsEq, IsEqArrow}
import cats.~>

trait MapKLaws[F[_], G[_], H[_], I[_]] {
  implicit def mapKInstance: MapK[F, G, H, I]
  implicit def unlift1Instance: Unlift[H, F]
  implicit def unlift2Instance: Unlift[I, G]

  // internal law:
  def mapKLiftFunctionKConsistency[A](ha: H[A], f: F ~> G): IsEq[I[A]] =
    mapKInstance.mapK(ha)(f) <-> mapKInstance.liftFunctionK(f)(ha)

  def mapKUnliftConsistency[A](ha: H[A], f: F ~> G): IsEq[Unlift.Result[G, A]] =
    unlift2Instance.unlift(mapKInstance.mapK(ha)(f)) <->
      unlift1Instance.unlift(ha).mapK(f)
}

object MapKLaws {
  def apply[F[_], G[_], H[_], I[_]](implicit
      mk: MapK[F, G, H, I],
      unlift1: Unlift[H, F],
      unlift2: Unlift[I, G],
  ): MapKLaws[F, G, H, I] =
    new MapKLaws[F, G, H, I] {
      implicit val mapKInstance: MapK[F, G, H, I] = mk
      implicit val unlift1Instance: Unlift[H, F] = unlift1
      implicit val unlift2Instance: Unlift[I, G] = unlift2
    }
}
