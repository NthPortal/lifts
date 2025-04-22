package lgbt.princess.lifts

import cats.data._
import cats.laws.discipline.arbitrary._
import cats.~>
import lgbt.princess.lifts.laws.discipline.MapKTests
import org.scalacheck.{Arbitrary, Gen}

class MapKLawTests extends BaseSuite {
  locally {
    import MapKTests.arbitraryFunctionKListVector

    checkAll("MapK[List, Vector, List, Vector]", MapKTests[List, Vector, List, Vector].mapK[String])
    checkAll(
      "MapK[List, Vector, OptionT[List, *], OptionT[Vector, *]]",
      MapKTests[List, Vector, OptionT[List, *], OptionT[Vector, *]].mapK[String]
    )
    checkAll(
      "MapK[List, Vector, EitherT[List, Int, *], EitherT[Vector, Int, *]]",
      MapKTests[List, Vector, EitherT[List, Int, *], EitherT[Vector, Int, *]].mapK[String]
    )
    checkAll(
      "MapK[List, Vector, IorT[List, Int, *], IorT[Vector, Int, *]]",
      MapKTests[List, Vector, IorT[List, Int, *], IorT[Vector, Int, *]].mapK[String]
    )
    checkAll(
      "MapK[List, Vector, Kleisli[List, Int, *], Kleisli[Vector, Int, *]]",
      MapKTests[List, Vector, Kleisli[List, Int, *], Kleisli[Vector, Int, *]].mapK[String]
    )
    checkAll(
      "MapK[List, Vector, WriterT[List, Int, *], WriterT[Vector, Int, *]]",
      MapKTests[List, Vector, WriterT[List, Int, *], WriterT[Vector, Int, *]].mapK[String]
    )
    checkAll(
      "MapK[List, Vector, OptionT[IorT[List, Int, *], *], OptionT[IorT[Vector, Int, *], *]]",
      MapKTests[List, Vector, OptionT[IorT[List, Int, *], *], OptionT[IorT[Vector, Int, *], *]]
        .mapK[String]
    )
  }

  locally {
    // `StateT` breaks up `List`s into `List`s of individual elements,
    // so `List ~> Vector` instances that change the collection's size don't work
    implicit val awfulBespokeArbListVector: Arbitrary[List ~> Vector] =
      Arbitrary {
        Gen.const {
          new (List ~> Vector) {
            def apply[A](fa: List[A]): Vector[A] =
              fa.map {
                case n: Int => (n + 1).asInstanceOf[A]
                case (s, n: Int) => (s, n + 1).asInstanceOf[A]
                case (e, s, n: Int) => (e, s, n + 1).asInstanceOf[A]
                case other => other
              }.toVector
          }
        }
      }

    checkAll(
      "MapK[List, Vector, StateT[List, String, *], StateT[Vector, String, *]]",
      MapKTests[List, Vector, StateT[List, String, *], StateT[Vector, String, *]].mapK[Int]
    )
    checkAll(
      "MapK[List, Vector, RWST[List, String, String, String, *], RWST[Vector, String, String, String, *]]",
      MapKTests[
        List,
        Vector,
        RWST[List, String, String, String, *],
        RWST[Vector, String, String, String, *]
      ].mapK[Int]
    )
  }
}
