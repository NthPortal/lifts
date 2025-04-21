package lgbt.princess.lifts
package laws
package discipline

import cats.{Eq, ~>}
import cats.kernel.laws.discipline.catsLawsIsEqToProp
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.{forAll => ∀}
import org.typelevel.discipline.Laws

trait LiftScopeTests[F[_], G[_]] extends Laws {
  implicit val liftInstance: LiftScope[F, G]

  def laws: LiftScopeLaws[F, G] = LiftScopeLaws[F, G]

  def liftScope[A](implicit
      arbGA: Arbitrary[G[A]],
      arbFF: Arbitrary[F ~> F],
      eqGA: Eq[G[A]],
  ): RuleSet =
    new SimpleRuleSet(
      name = "liftScope",
      "limitedMapK with identity is pure" -> ∀(laws.limitedMapKIdentityIsPure[A] _),
      "limitedMapK and liftScope are consistent" -> ∀(laws.limitedMapKLiftScopeConsistency[A] _),
    )
}

object LiftScopeTests {
  def apply[F[_], G[_]](implicit lift: LiftScope[F, G]): LiftScopeTests[F, G] =
    new LiftScopeTests[F, G] {
      implicit val liftInstance: LiftScope[F, G] = lift
    }

  implicit val arbitraryFunctionKListList: Arbitrary[List ~> List] =
    Arbitrary(ListGens.genFunctionKListList)
}
