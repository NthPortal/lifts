package lgbt.princess.lifts
package laws

import cats.laws.{IsEq, IsEqArrow}
import cats.~>

trait MapKLaws[F[_], G[_], H[_], I[_]] {
  implicit def mapKInstance: MapK[F, G, H, I]
  implicit def unliftHFInstance: Unlift[H, F]
  implicit def unliftIGInstance: Unlift[I, G]

  // internal law:
  def mapKLiftFunctionKConsistency[A](ha: H[A], f: F ~> G): IsEq[I[A]] =
    mapKInstance.mapK(ha)(f) <-> mapKInstance.liftFunctionK(f)(ha)

  def mapKUnliftConsistency[A](ha: H[A], f: F ~> G): IsEq[Unlift.Result[G, A]] =
    unliftIGInstance.unlift(mapKInstance.mapK(ha)(f)) <->
      unliftHFInstance.unlift(ha).mapK(f)
}

object MapKLaws {
  def apply[F[_], G[_], H[_], I[_]](implicit
      mk: MapK[F, G, H, I],
      unliftHF: Unlift[H, F],
      unliftIG: Unlift[I, G]
  ): MapKLaws[F, G, H, I] =
    new MapKLaws[F, G, H, I] {
      implicit val mapKInstance: MapK[F, G, H, I] = mk
      implicit val unliftHFInstance: Unlift[H, F] = unliftHF
      implicit val unliftIGInstance: Unlift[I, G] = unliftIG
    }
}
