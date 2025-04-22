package lgbt.princess.lifts
package laws

import cats.laws.{IsEq, IsEqArrow}
import cats.~>

trait MapKLaws[F[_], G[_], H[_], I[_]] {
  implicit def mapKInstance: MapK[F, G, H, I]

  // internal law:
  def mapKLiftFunctionKConsistency[A](ha: H[A], f: F ~> G): IsEq[I[A]] =
    mapKInstance.mapK(ha)(f) <-> mapKInstance.liftFunctionK(f)(ha)
}

object MapKLaws {
  def apply[F[_], G[_], H[_], I[_]](implicit mk: MapK[F, G, H, I]): MapKLaws[F, G, H, I] =
    new MapKLaws[F, G, H, I] {
      implicit val mapKInstance: MapK[F, G, H, I] = mk
    }
}
