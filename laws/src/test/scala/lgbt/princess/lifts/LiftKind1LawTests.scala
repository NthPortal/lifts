package lgbt.princess.lifts

import cats.data._
import cats.laws.discipline.arbitrary._
import cats.~>
import lgbt.princess.lifts.laws.discipline.LiftKind1Tests
import org.scalacheck.{Arbitrary, Gen}

class LiftKind1LawTests extends BaseSuite {
  locally {
    import LiftKind1Tests.arbitraryFunctionKListList

    checkAll("LiftKind1[List, List]", LiftKind1Tests[List, List].liftKind1[String])
    checkAll(
      "LiftKind1[List, OptionT[List, *]]",
      LiftKind1Tests[List, OptionT[List, *]].liftKind1[String]
    )
    checkAll(
      "LiftKind1[List, EitherT[List, Int, *]]",
      LiftKind1Tests[List, EitherT[List, Int, *]].liftKind1[String]
    )
    checkAll(
      "LiftKind1[List, IorT[List, Int, *]]",
      LiftKind1Tests[List, IorT[List, Int, *]].liftKind1[String]
    )
    checkAll(
      "LiftKind1[List, Kleisli[List, Int, *]]",
      LiftKind1Tests[List, Kleisli[List, Int, *]].liftKind1[String]
    )
    checkAll(
      "LiftKind1[List, WriterT[List, Int, *]]",
      LiftKind1Tests[List, WriterT[List, Int, *]].liftKind1[String]
    )
    checkAll(
      "LiftKind1[List, OptionT[IorT[List, Int, *], *]]",
      LiftKind1Tests[List, OptionT[IorT[List, Int, *], *]].liftKind1[String]
    )
  }

  locally {
    // `StateT` breaks up `List`s into `List`s of individual elements,
    // so `List ~> List` instances that change the `List`'s size don't work
    implicit val awfulBespokeArbListList: Arbitrary[List ~> List] =
      Arbitrary {
        Gen.const {
          new (List ~> List) {
            def apply[A](fa: List[A]): List[A] =
              fa.map {
                case n: Int => (n + 1).asInstanceOf[A]
                case (s, n: Int) => (s, n + 1).asInstanceOf[A]
                case (e, s, n: Int) => (e, s, n + 1).asInstanceOf[A]
                case Right(n: Int) => Right(n + 1).asInstanceOf[A]
                case other => other
              }
          }
        }
      }

    checkAll(
      "LiftKind1[List, StateT[List, String, *]]",
      LiftKind1Tests[List, StateT[List, String, *]].liftKind1[Int]
    )
    checkAll(
      "LiftKind1[List, RWST[List, String, String, String, *]]",
      LiftKind1Tests[List, RWST[List, String, String, String, *]].liftKind1[Int]
    )
  }
}
