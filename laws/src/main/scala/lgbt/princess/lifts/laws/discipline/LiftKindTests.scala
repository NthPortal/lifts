package lgbt.princess.lifts
package laws
package discipline

import cats.{Eq, ~>}
import cats.kernel.laws.discipline.catsLawsIsEqToProp
import org.scalacheck.{Arbitrary, Prop}
import org.scalacheck.Prop.{forAll => ∀}
import org.typelevel.discipline.Laws

trait LiftKindTests[From[_], To[_]] extends LiftValueTests[From, To] with LiftScopeTests[From, To] {
  implicit val liftInstance: LiftKind[From, To]
  implicit val unliftInstance: Unlift[To, From]

  override def laws: LiftKindLaws[From, To] = LiftKindLaws[From, To]

  def liftKind[A](implicit
      arbFA: Arbitrary[From[A]],
      arbGA: Arbitrary[To[A]],
      arbFF: Arbitrary[From ~> From],
      eqFA: Eq[Unlift.Result[From, A]],
      eqGA: Eq[To[A]],
  ): RuleSet =
    new RuleSet {
      def name: String = "liftKind"
      val parents: Seq[RuleSet] = Seq(liftValue[A], liftScope[A])
      def bases: Seq[(String, Laws#RuleSet)] = Seq.empty
      val props: Seq[(String, Prop)] = Seq(
        "liftF and limitedMapK are consistent" -> ∀(laws.liftFLimitedMapKConsistency[A] _),
        "limitedMapK is reversible" -> ∀(laws.limitedMapKIsReversible[A] _),
      )
    }
}

object LiftKindTests {
  def apply[From[_], To[_]](implicit
      lift: LiftKind[From, To],
      unlift: Unlift[To, From],
  ): LiftKindTests[From, To] =
    new LiftKindTests[From, To] {
      implicit val liftInstance: LiftKind[From, To] = lift
      implicit val unliftInstance: Unlift[To, From] = unlift
    }

  implicit def arbitraryFunctionKListList: Arbitrary[List ~> List] =
    LiftScopeTests.arbitraryFunctionKListList
}
