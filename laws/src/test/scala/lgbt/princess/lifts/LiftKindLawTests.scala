package lgbt.princess.lifts

import cats.data._
import cats.laws.discipline.arbitrary._
import cats.~>
import lgbt.princess.lifts.laws.discipline.LiftKindTests
import org.scalacheck.{Arbitrary, Gen}

class LiftKindLawTests extends BaseSuite {
  locally {
    import LiftKindTests.arbitraryFunctionKListList

    checkAll("LiftKind[List, List]", LiftKindTests[List, List].liftKind[String])
    checkAll(
      "LiftKind[List, OptionT[List, *]]",
      LiftKindTests[List, OptionT[List, *]].liftKind[String]
    )
    checkAll(
      "LiftKind[List, EitherT[List, Int, *]]",
      LiftKindTests[List, EitherT[List, Int, *]].liftKind[String]
    )
    checkAll(
      "LiftKind[List, IorT[List, Int, *]]",
      LiftKindTests[List, IorT[List, Int, *]].liftKind[String]
    )
    checkAll(
      "LiftKind[List, Kleisli[List, Int, *]]",
      LiftKindTests[List, Kleisli[List, Int, *]].liftKind[String]
    )
    checkAll(
      "LiftKind[List, WriterT[List, Int, *]]",
      LiftKindTests[List, WriterT[List, Int, *]].liftKind[String]
    )
    checkAll(
      "LiftKind[List, OptionT[IorT[List, Int, *], *]]",
      LiftKindTests[List, OptionT[IorT[List, Int, *], *]].liftKind[String]
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
                case other => other
              }
          }
        }
      }

    checkAll(
      "LiftKind[List, StateT[List, String, *]]",
      LiftKindTests[List, StateT[List, String, *]].liftKind[Int]
    )
  }
}
