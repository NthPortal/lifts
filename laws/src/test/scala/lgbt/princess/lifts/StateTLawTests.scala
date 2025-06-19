package lgbt.princess.lifts

import cats.{Eq, FlatMap, ~>}
import cats.data.StateT
import cats.laws.discipline.arbitrary._
import cats.laws.discipline.eq._
import lgbt.princess.lifts.laws.discipline.MapKTests
import org.scalacheck.{Arbitrary, Gen}

class StateTLawTests extends BaseSuite {
  import lgbt.princess.lifts.MapK.MonadMorphismOnly.stateT

  implicit val arbitraryFunctionKListVector: Arbitrary[List ~> Vector] =
    Arbitrary {
      Gen.const {
        new (List ~> Vector) {
          def apply[A](fa: List[A]): Vector[A] = fa.toVector
        }
      }
    }

  implicit def eqStateT[F[_]: FlatMap, S, A](implicit
      arbS: Arbitrary[S],
      eqFSA: Eq[F[(S, A)]]
  ): Eq[StateT[F, S, A]] =
    Eq.by(state => (s: S) => state.run(s))

  checkAll(
    "MapK[List, Vector, StateT[List, Int, *], StateT[Vector, Int, *]]",
    MapKTests[List, Vector, StateT[List, Int, *], StateT[Vector, Int, *]].mapK[String]
  )
}
