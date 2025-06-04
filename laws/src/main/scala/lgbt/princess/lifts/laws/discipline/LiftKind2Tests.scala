package lgbt.princess.lifts
package laws
package discipline

import cats.{Eq, ~>}
import cats.kernel.laws.discipline.catsLawsIsEqToProp
import org.scalacheck.{Arbitrary, Prop}
import org.scalacheck.Prop.{forAll => ∀}
import org.typelevel.discipline.Laws

trait LiftKind2Tests[In1[_], Out1[_], In2[_], Out2[_]] extends MapKTests[In1, Out1, In2, Out2] {
  implicit val liftInstance: LiftKind2[In1, Out1, In2, Out2]
  implicit final def mapKInstance: MapK[In1, Out1, In2, Out2] = liftInstance

  override def laws: LiftKind2Laws[In1, Out1, In2, Out2] = LiftKind2Laws[In1, Out1, In2, Out2]

  private def liftValueInTests: LiftValueTests[In1, In2] = LiftValueTests[In1, In2]
  private def liftValueOutTests: LiftValueTests[Out1, Out2] = LiftValueTests[Out1, Out2]

  def liftKind2[A](implicit
      arbFA: Arbitrary[In1[A]],
      arbGA: Arbitrary[Out1[A]],
      arbHA: Arbitrary[In2[A]],
      arbFG: Arbitrary[In1 ~> Out1],
      eqFA: Eq[Unlift.Result[In1, A]],
      eqGA: Eq[Unlift.Result[Out1, A]],
      eqHA: Eq[In2[A]],
      eqIA: Eq[Out2[A]],
  ): RuleSet =
    new RuleSet with HasOneParent {
      def name: String = "liftKind2"
      override val parent: Option[RuleSet] = Some(mapK[A])
      val bases: Seq[(String, Laws#RuleSet)] = Seq(
        "liftValueIn" -> liftValueInTests.liftValue[A],
        "liftValueOut" -> liftValueOutTests.liftValue[A],
      )
      val props: Seq[(String, Prop)] = Seq(
        "liftF and mapK are consistent" -> ∀(laws.liftFMapKConsistency[A] _),
        "mapK is reversible" -> ∀(laws.mapKIsReversible[A] _),
      )
    }
}

object LiftKind2Tests {
  def apply[In1[_], Out1[_], In2[_], Out2[_]](implicit
      lift: LiftKind2[In1, Out1, In2, Out2],
      unliftIn: Unlift[In2, In1],
      unliftOut: Unlift[Out2, Out1],
  ): LiftKind2Tests[In1, Out1, In2, Out2] = {
    new LiftKind2Tests[In1, Out1, In2, Out2] {
      implicit val liftInstance: LiftKind2[In1, Out1, In2, Out2] = lift
      implicit val unliftInInstance: Unlift[In2, In1] = unliftIn
      implicit val unliftOutInstance: Unlift[Out2, Out1] = unliftOut
    }
  }

  implicit def arbitraryFunctionKListVector: Arbitrary[List ~> Vector] =
    MapKTests.arbitraryFunctionKListVector
}
