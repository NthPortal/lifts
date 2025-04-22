package lgbt.princess.lifts
package laws
package discipline

import cats.{Eq, ~>}
import cats.kernel.laws.discipline.catsLawsIsEqToProp
import org.scalacheck.{Arbitrary, Prop}
import org.scalacheck.Prop.{forAll => ∀}
import org.typelevel.discipline.Laws

trait LiftKind1Tests[F[_], G[_]] extends LiftValueTests[F, G] with MapKTests[F, F, G, G] {
  implicit val liftInstance: LiftKind1[F, G]
  implicit final def mapKInstance: MapK[F, F, G, G] = liftInstance
  implicit val unliftInstance: Unlift[G, F]

  override def laws: LiftKind1Laws[F, G] = LiftKind1Laws[F, G]

  def liftKind1[A](implicit
      arbFA: Arbitrary[F[A]],
      arbGA: Arbitrary[G[A]],
      arbFF: Arbitrary[F ~> F],
      eqFA: Eq[Unlift.Result[F, A]],
      eqGA: Eq[G[A]],
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
  def apply[F[_], G[_]](implicit
      lift: LiftKind1[F, G],
      unlift: Unlift[G, F]
  ): LiftKind1Tests[F, G] =
    new LiftKind1Tests[F, G] {
      implicit val liftInstance: LiftKind1[F, G] = lift
      implicit val unliftInstance: Unlift[G, F] = unlift
      implicit def unlift1Instance: Unlift[G, F] = unliftInstance
      implicit def unlift2Instance: Unlift[G, F] = unliftInstance
    }

  implicit def arbitraryFunctionKListList: Arbitrary[List ~> List] =
    LiftScopeTests.arbitraryFunctionKListList
}
