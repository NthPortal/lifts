package lgbt.princess.lifts
package laws
package discipline

import cats.Eq
import cats.kernel.laws.discipline.catsLawsIsEqToProp
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.{forAll => ∀}
import org.typelevel.discipline.Laws

trait LiftValueTests[F[_], G[_]] extends Laws {
  implicit val liftInstance: LiftValue[F, G]
  implicit val unliftInstance: Unlift[G, F]

  def laws: LiftValueLaws[F, G] = LiftValueLaws[F, G]

  def liftValue[A](implicit
      arbFA: Arbitrary[F[A]],
      eqFA: Eq[Unlift.Result[F, A]],
      eqGA: Eq[G[A]],
  ): RuleSet =
    new SimpleRuleSet(
      name = "liftValue",
      "liftF and liftK are consistent" -> ∀(laws.liftFLiftKConsistency[A] _),
      "liftF is reversible" -> ∀(laws.liftFIsReversible[A] _),
    )
}

object LiftValueTests {
  def apply[F[_], G[_]](implicit
      lift: LiftValue[F, G],
      unlift: Unlift[G, F],
  ): LiftValueTests[F, G] =
    new LiftValueTests[F, G] {
      implicit val liftInstance: LiftValue[F, G] = lift
      implicit val unliftInstance: Unlift[G, F] = unlift
    }
}
