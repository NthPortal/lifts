package lgbt.princess.lifts
package laws
package discipline

import cats.{Eq, ~>}
import cats.kernel.laws.discipline.catsLawsIsEqToProp
import org.scalacheck.{Arbitrary, Prop}
import org.scalacheck.Prop.{forAll => ∀}
import org.typelevel.discipline.Laws

trait LiftKind2Tests[F[_], G[_], H[_], I[_]] extends MapKTests[F, G, H, I] {
  implicit val liftInstance: LiftKind2[F, G, H, I]
  implicit final def mapKInstance: MapK[F, G, H, I] = liftInstance

  override def laws: LiftKind2Laws[F, G, H, I] = LiftKind2Laws[F, G, H, I]

  private def liftValue1Tests: LiftValueTests[F, H] = LiftValueTests[F, H]
  private def liftValue2Tests: LiftValueTests[G, I] = LiftValueTests[G, I]

  def liftKind2[A](implicit
      arbFA: Arbitrary[F[A]],
      arbGA: Arbitrary[G[A]],
      arbHA: Arbitrary[H[A]],
      arbFG: Arbitrary[F ~> G],
      eqFA: Eq[Unlift.Result[F, A]],
      eqGA: Eq[Unlift.Result[G, A]],
      eqHA: Eq[H[A]],
      eqIA: Eq[I[A]],
  ): RuleSet =
    new RuleSet with HasOneParent {
      def name: String = "liftKind2"
      override val parent: Option[RuleSet] = Some(mapK[A])
      val bases: Seq[(String, Laws#RuleSet)] = Seq(
        "liftValue1" -> liftValue1Tests.liftValue[A],
        "liftValue2" -> liftValue2Tests.liftValue[A],
      )
      val props: Seq[(String, Prop)] = Seq(
        "liftF and mapK are consistent" -> ∀(laws.liftFMapKConsistency[A] _),
        "mapK is reversible" -> ∀(laws.mapKIsReversible[A] _),
      )
    }
}

object LiftKind2Tests {
  def apply[F[_], G[_], H[_], I[_]](implicit
      lift: LiftKind2[F, G, H, I],
      unlift1: Unlift[H, F],
      unlift2: Unlift[I, G],
  ): LiftKind2Tests[F, G, H, I] = {
    new LiftKind2Tests[F, G, H, I] {
      implicit val liftInstance: LiftKind2[F, G, H, I] = lift
      implicit val unlift1Instance: Unlift[H, F] = unlift1
      implicit val unlift2Instance: Unlift[I, G] = unlift2
    }
  }

  implicit def arbitraryFunctionKListVector: Arbitrary[List ~> Vector] =
    MapKTests.arbitraryFunctionKListVector
}
