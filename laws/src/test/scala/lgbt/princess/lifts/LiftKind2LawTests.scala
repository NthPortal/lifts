package lgbt.princess.lifts

import cats.data._
import cats.laws.discipline.arbitrary._
import cats.~>
import lgbt.princess.lifts.laws.discipline.LiftKind2Tests
import org.scalacheck.{Arbitrary, Gen}

class LiftKind2LawTests extends BaseSuite {
  locally {
    import LiftKind2Tests.arbitraryFunctionKListVector

    checkAll(
      "LiftKind2[List, Vector, List, Vector]",
      LiftKind2Tests[List, Vector, List, Vector].liftKind2[String]
    )
    checkAll(
      "LiftKind2[List, Vector, OptionT[List, *], OptionT[Vector, *]]",
      LiftKind2Tests[List, Vector, OptionT[List, *], OptionT[Vector, *]].liftKind2[String]
    )
    checkAll(
      "LiftKind2[List, Vector, EitherT[List, Int, *], EitherT[Vector, Int, *]]",
      LiftKind2Tests[List, Vector, EitherT[List, Int, *], EitherT[Vector, Int, *]].liftKind2[String]
    )
    checkAll(
      "LiftKind2[List, Vector, IorT[List, Int, *], IorT[Vector, Int, *]]",
      LiftKind2Tests[List, Vector, IorT[List, Int, *], IorT[Vector, Int, *]].liftKind2[String]
    )
    checkAll(
      "LiftKind2[List, Vector, Kleisli[List, Int, *], Kleisli[Vector, Int, *]]",
      LiftKind2Tests[List, Vector, Kleisli[List, Int, *], Kleisli[Vector, Int, *]].liftKind2[String]
    )
    checkAll(
      "LiftKind2[List, Vector, WriterT[List, Int, *], WriterT[Vector, Int, *]]",
      LiftKind2Tests[List, Vector, WriterT[List, Int, *], WriterT[Vector, Int, *]].liftKind2[String]
    )
    checkAll(
      "LiftKind2[List, Vector, OptionT[IorT[List, Int, *], *], OptionT[IorT[Vector, Int, *], *]]",
      LiftKind2Tests[List, Vector, OptionT[IorT[List, Int, *], *], OptionT[IorT[Vector, Int, *], *]]
        .liftKind2[String]
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
      "LiftKind2[List, Vector, StateT[List, String, *], StateT[Vector, String, *]]",
      LiftKind2Tests[List, Vector, StateT[List, String, *], StateT[Vector, String, *]]
        .liftKind2[Int]
    )
    checkAll(
      "LiftKind2[List, Vector, RWST[List, String, String, String, *], RWST[Vector, String, String, String, *]]",
      LiftKind2Tests[
        List,
        Vector,
        RWST[List, String, String, String, *],
        RWST[Vector, String, String, String, *]
      ].liftKind2[Int]
    )
  }
}
