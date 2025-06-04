package lgbt.princess.lifts
package laws
package discipline

import cats.{Eq, ~>}
import cats.kernel.laws.discipline.catsLawsIsEqToProp
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.{forAll => ∀}
import org.typelevel.discipline.Laws

trait LiftScopeTests[From[_], To[_]] extends Laws {
  implicit val liftInstance: LiftScope[From, To]

  def laws: LiftScopeLaws[From, To] = LiftScopeLaws[From, To]

  def liftScope[A](implicit
      arbGA: Arbitrary[To[A]],
      arbFF: Arbitrary[From ~> From],
      eqGA: Eq[To[A]],
  ): RuleSet =
    new SimpleRuleSet(
      name = "liftScope",
      "limitedMapK with identity is pure" -> ∀(laws.limitedMapKIdentityIsPure[A] _),
      "limitedMapK and liftScope are consistent" -> ∀(laws.limitedMapKLiftScopeConsistency[A] _),
    )
}

object LiftScopeTests {
  def apply[From[_], To[_]](implicit lift: LiftScope[From, To]): LiftScopeTests[From, To] =
    new LiftScopeTests[From, To] {
      implicit val liftInstance: LiftScope[From, To] = lift
    }

  implicit val arbitraryFunctionKListList: Arbitrary[List ~> List] =
    Arbitrary(ListGens.genFunctionKListList)
}
