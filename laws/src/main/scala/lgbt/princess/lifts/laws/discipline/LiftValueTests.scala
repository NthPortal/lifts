package lgbt.princess.lifts
package laws
package discipline

import cats.Eq
import cats.kernel.laws.discipline.catsLawsIsEqToProp
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.{forAll => ∀}
import org.typelevel.discipline.Laws

trait LiftValueTests[From[_], To[_]] extends Laws {
  implicit val liftInstance: LiftValue[From, To]
  implicit val unliftInstance: Unlift[To, From]

  def laws: LiftValueLaws[From, To] = LiftValueLaws[From, To]

  def liftValue[A](implicit
      arbFA: Arbitrary[From[A]],
      eqFA: Eq[Unlift.Result[From, A]],
      eqGA: Eq[To[A]],
  ): RuleSet =
    new SimpleRuleSet(
      name = "liftValue",
      "liftF and liftK are consistent" -> ∀(laws.liftFLiftKConsistency[A] _),
      "liftF is reversible" -> ∀(laws.liftFIsReversible[A] _),
    )
}

object LiftValueTests {
  def apply[From[_], To[_]](implicit
      lift: LiftValue[From, To],
      unlift: Unlift[To, From],
  ): LiftValueTests[From, To] =
    new LiftValueTests[From, To] {
      implicit val liftInstance: LiftValue[From, To] = lift
      implicit val unliftInstance: Unlift[To, From] = unlift
    }
}
