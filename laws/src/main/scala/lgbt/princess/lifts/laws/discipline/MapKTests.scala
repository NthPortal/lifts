package lgbt.princess.lifts
package laws
package discipline

import cats.{Eq, ~>}
import cats.kernel.laws.discipline.catsLawsIsEqToProp
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.{forAll => ∀}
import org.typelevel.discipline.Laws

trait MapKTests[In1[_], Out1[_], In2[_], Out2[_]] extends Laws {
  implicit def mapKInstance: MapK[In1, Out1, In2, Out2]
  implicit def unliftInInstance: Unlift[In2, In1]
  implicit def unliftOutInstance: Unlift[Out2, Out1]

  def laws: MapKLaws[In1, Out1, In2, Out2] = MapKLaws[In1, Out1, In2, Out2]

  def mapK[A](implicit
      arbHA: Arbitrary[In2[A]],
      arbFG: Arbitrary[In1 ~> Out1],
      eqGA: Eq[Unlift.Result[Out1, A]],
      eqIA: Eq[Out2[A]],
  ): RuleSet =
    new SimpleRuleSet(
      name = "mapK",
      "mapK and liftFunctionK are consistent" -> ∀(laws.mapKLiftFunctionKConsistency[A] _),
      "mapK and unlift are consistent" -> ∀(laws.mapKUnliftConsistency[A]),
    )
}

object MapKTests {
  def apply[In1[_], Out1[_], In2[_], Out2[_]](implicit
      mk: MapK[In1, Out1, In2, Out2],
      unliftIn: Unlift[In2, In1],
      unliftOut: Unlift[Out2, Out1],
  ): MapKTests[In1, Out1, In2, Out2] =
    new MapKTests[In1, Out1, In2, Out2] {
      implicit val mapKInstance: MapK[In1, Out1, In2, Out2] = mk
      implicit val unliftInInstance: Unlift[In2, In1] = unliftIn
      implicit val unliftOutInstance: Unlift[Out2, Out1] = unliftOut
    }

  implicit val arbitraryFunctionKListVector: Arbitrary[List ~> Vector] =
    Arbitrary(ListGens.genFunctionKListVector)
}
