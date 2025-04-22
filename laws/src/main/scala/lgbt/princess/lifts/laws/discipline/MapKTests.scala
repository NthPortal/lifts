package lgbt.princess.lifts
package laws
package discipline

import cats.{Eq, ~>}
import cats.kernel.laws.discipline.catsLawsIsEqToProp
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.{forAll => ∀}
import org.typelevel.discipline.Laws

trait MapKTests[F[_], G[_], H[_], I[_]] extends Laws {
  implicit def mapKInstance: MapK[F, G, H, I]
  implicit def unliftHFInstance: Unlift[H, F]
  implicit def unliftIGInstance: Unlift[I, G]

  def laws: MapKLaws[F, G, H, I] = MapKLaws[F, G, H, I]

  def mapK[A](implicit
      arbHA: Arbitrary[H[A]],
      arbFG: Arbitrary[F ~> G],
      eqGA: Eq[Unlift.Result[G, A]],
      eqIA: Eq[I[A]],
  ): RuleSet =
    new SimpleRuleSet(
      name = "mapK",
      "mapK and liftFunctionK are consistent" -> ∀(laws.mapKLiftFunctionKConsistency[A] _),
      "mapK and unlift are consistent" -> ∀(laws.mapKUnliftConsistency[A]),
    )
}

object MapKTests {
  def apply[F[_], G[_], H[_], I[_]](implicit
      mk: MapK[F, G, H, I],
      unliftHF: Unlift[H, F],
      unliftIG: Unlift[I, G],
  ): MapKTests[F, G, H, I] =
    new MapKTests[F, G, H, I] {
      implicit val mapKInstance: MapK[F, G, H, I] = mk
      implicit val unliftHFInstance: Unlift[H, F] = unliftHF
      implicit val unliftIGInstance: Unlift[I, G] = unliftIG
    }

  implicit val arbitraryFunctionKListVector: Arbitrary[List ~> Vector] =
    Arbitrary(ListGens.genFunctionKListVector)
}
