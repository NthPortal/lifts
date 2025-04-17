package lgbt.princess.lifts
package laws
package discipline

import cats.{Eq, ~>}
import cats.kernel.laws.discipline.catsLawsIsEqToProp
import org.scalacheck.{Arbitrary, Prop}
import org.scalacheck.Prop.{forAll => ∀}
import org.typelevel.discipline.Laws

trait LiftKindTests[F[_], G[_]] extends LiftValueTests[F, G] with LiftScopeTests[F, G] {
  implicit val liftInstance: LiftKind[F, G]
  implicit val unliftInstance: Unlift[G, F]

  override def laws: LiftKindLaws[F, G] = LiftKindLaws[F, G]

  def liftKind[A](implicit
      arbFA: Arbitrary[F[A]],
      arbGA: Arbitrary[G[A]],
      arbFF: Arbitrary[F ~> F],
      eqFA: Eq[Unlift.Result[F, A]],
      eqGA: Eq[G[A]],
  ): RuleSet =
    new RuleSet {
      def name: String = "liftKind"
      val parents: Seq[RuleSet] = Seq(liftValue[A], liftScope[A])
      def bases: Seq[(String, Laws#RuleSet)] = Seq.empty
      val props: Seq[(String, Prop)] = Seq(
        "liftF and liftScopeApply are consistent" -> ∀(laws.liftFLiftScopeApplyConsistency[A] _),
        "liftScopeApply is reversible" -> ∀(laws.liftScopeApplyIsReversible[A] _),
      )
    }
}

object LiftKindTests {
  def apply[F[_], G[_]](implicit
      lift: LiftKind[F, G],
      unlift: Unlift[G, F],
  ): LiftKindTests[F, G] =
    new LiftKindTests[F, G] {
      implicit val liftInstance: LiftKind[F, G] = lift
      implicit val unliftInstance: Unlift[G, F] = unlift
    }

  implicit def arbitraryFunctionKListList: Arbitrary[List ~> List] =
    LiftScopeTests.arbitraryFunctionKListList
}
