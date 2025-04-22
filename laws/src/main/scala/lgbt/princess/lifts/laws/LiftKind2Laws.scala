package lgbt.princess.lifts
package laws

import cats.laws.IsEq
import cats.laws.IsEqArrow
import cats.{Functor, ~>}

trait LiftKind2Laws[F[_], G[_], H[_], I[_]] extends MapKLaws[F, G, H, I] {
  implicit def liftInstance: LiftKind2[F, G, H, I]
  implicit final def mapKInstance: MapK[F, G, H, I] = liftInstance
  def liftValue1Laws: LiftValueLaws[F, H]
  def liftValue2Laws: LiftValueLaws[G, I]
  implicit def functorG: Functor[G] = liftValue2Laws.functor

  // internal laws:
  def liftFMapKConsistency[A](fa: F[A], f: F ~> G): IsEq[I[A]] =
    liftInstance.liftValue2.liftF(f(fa)) <->
      liftInstance.mapK(liftInstance.liftValue1.liftF(fa))(f)

  def mapKIsReversible[A](fa: F[A], f: F ~> G): IsEq[Unlift.Result[G, A]] =
    liftValue2Laws.unliftInstance.unlift {
      liftInstance.mapK(liftInstance.liftValue1.liftF(fa))(f)
    } <-> Unlift.success(f(fa))
}

object LiftKind2Laws {
  def apply[F[_], G[_], H[_], I[_]](implicit
      lift: LiftKind2[F, G, H, I],
      unlift1: Unlift[H, F],
      unlift2: Unlift[I, G],
  ): LiftKind2Laws[F, G, H, I] = {
    val lv1Laws = LiftValueLaws[F, H]
    val lv2Laws = LiftValueLaws[G, I]
    new LiftKind2Laws[F, G, H, I] {
      implicit val liftInstance: LiftKind2[F, G, H, I] = lift
      val liftValue1Laws: LiftValueLaws[F, H] = lv1Laws
      val liftValue2Laws: LiftValueLaws[G, I] = lv2Laws
      implicit def unlift1Instance: Unlift[H, F] = liftValue1Laws.unliftInstance
      implicit def unlift2Instance: Unlift[I, G] = liftValue2Laws.unliftInstance
    }
  }
}
