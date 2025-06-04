package lgbt.princess.lifts
package laws
package discipline

import cats.{Eq, ~>}
import cats.kernel.laws.discipline.catsLawsIsEqToProp
import org.scalacheck.{Arbitrary, Prop}
import org.scalacheck.Prop.{forAll => ∀}
import org.typelevel.discipline.Laws

trait LiftKind1Tests[From[_], To[_]]
    extends LiftValueTests[From, To]
    with MapKTests[From, From, To, To] {
  implicit val liftInstance: LiftKind1[From, To]
  implicit final def mapKInstance: MapK[From, From, To, To] = liftInstance
  implicit val unliftInstance: Unlift[To, From]

  override def laws: LiftKind1Laws[From, To] = LiftKind1Laws[From, To]

  def liftKind1[A](implicit
      arbFA: Arbitrary[From[A]],
      arbGA: Arbitrary[To[A]],
      arbFF: Arbitrary[From ~> From],
      eqFA: Eq[Unlift.Result[From, A]],
      eqGA: Eq[To[A]],
  ): RuleSet =
    new RuleSet {
      def name: String = "liftKind1"
      val parents: Seq[RuleSet] = Seq(liftValue[A], mapK[A])
      def bases: Seq[(String, Laws#RuleSet)] = Seq.empty
      val props: Seq[(String, Prop)] = Seq(
        "mapK with identity is pure" -> ∀(laws.mapKIdentityIsPure[A] _),
        "liftF and mapK are consistent" -> ∀(laws.liftFMapKConsistency[A] _),
        "mapK is reversible" -> ∀(laws.mapKIsReversible[A] _),
      )
    }
}

object LiftKind1Tests {
  def apply[From[_], To[_]](implicit
      lift: LiftKind1[From, To],
      unlift: Unlift[To, From]
  ): LiftKind1Tests[From, To] =
    new LiftKind1Tests[From, To] {
      implicit val liftInstance: LiftKind1[From, To] = lift
      implicit val unliftInstance: Unlift[To, From] = unlift
      implicit def unliftInInstance: Unlift[To, From] = unliftInstance
      implicit def unliftOutInstance: Unlift[To, From] = unliftInstance
    }

  implicit def arbitraryFunctionKListList: Arbitrary[List ~> List] =
    LiftScopeTests.arbitraryFunctionKListList
}
