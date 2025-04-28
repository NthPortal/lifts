package lgbt.princess.lifts
package instances

import cats.data.OptionT
import cats.effect._
import cats.effect.kernel.testkit.PureConcGenerators._
import cats.effect.kernel.testkit.pure._
import cats.laws.discipline.arbitrary.catsLawsArbitraryForOptionT
import cats.syntax.flatMap._
import cats.{Eq, ~>}
import lgbt.princess.lifts.instances.ResourceInstances._
import lgbt.princess.lifts.laws.discipline._
import org.scalacheck.{Arbitrary, Gen}

class ResourceLawTests extends CESuite {
  type PCT[A] = PureConc[Throwable, A]

  val counter: PCT[Ref[PCT, Int]] = Concurrent[PCT].ref(0)

  implicit val eqThrowable: Eq[Throwable] = Eq.fromUniversalEquals

  implicit val arbitraryScope: Arbitrary[PCT ~> PCT] =
    Arbitrary {
      Gen.const {
        new (PCT ~> PCT) {
          def apply[A](fa: PCT[A]): PCT[A] =
            for {
              ref <- counter
              res <- ref.update(_ + 1) >> fa
            } yield res
        }
      }
    }

  implicit val arbitraryOptionTScope: Arbitrary[PCT ~> OptionT[PCT, *]] =
    Arbitrary {
      arbitraryScope.arbitrary.map {
        _.andThen {
          new (PCT ~> OptionT[PCT, *]) {
            def apply[A](fa: PCT[A]): OptionT[PCT, A] = OptionT.liftF(fa)
          }
        }
      }
    }

  implicit def arbitraryResource[F[_], A](implicit
      arbFA: Arbitrary[F[A]]
  ): Arbitrary[Resource[F, A]] =
    Arbitrary(arbFA.arbitrary.map(Resource.eval))

  checkAll(
    "LiftValue[PCT, Resource[PCT, *]]",
    LiftValueTests[PCT, Resource[PCT, *]].liftValue[Int]
  )
  checkAll(
    "LiftScope[PCT, Resource[PCT, *]]",
    LiftScopeTests[PCT, Resource[PCT, *]].liftScope[Int]
  )
  checkAll(
    "LiftKind[PCT, Resource[PCT, *]]",
    LiftKindTests[PCT, Resource[PCT, *]].liftKind[Int]
  )
  checkAll(
    "MapK[PCT, OptionT[PCT, *], Resource[PCT, *], Resource[OptionT[PCT, *], *]]",
    MapKTests[PCT, OptionT[PCT, *], Resource[PCT, *], Resource[OptionT[PCT, *], *]]
      .mapK[Int]
  )
  checkAll(
    "LiftKind1[PCT, Resource[PCT, *]]",
    LiftKind1Tests[PCT, Resource[PCT, *]].liftKind1[Int]
  )
  checkAll(
    "LiftKind2[PCT, OptionT[PCT, *], Resource[PCT, *], Resource[OptionT[PCT, *], *]]",
    LiftKind2Tests[PCT, OptionT[PCT, *], Resource[PCT, *], Resource[OptionT[PCT, *], *]]
      .liftKind2[Int]
  )
}
