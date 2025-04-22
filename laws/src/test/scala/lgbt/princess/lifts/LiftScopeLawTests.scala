package lgbt.princess.lifts

import cats.data._
import cats.laws.discipline.arbitrary._
import cats.~>
import lgbt.princess.lifts.laws.discipline.LiftScopeTests
import org.scalacheck.{Arbitrary, Gen}

class LiftScopeLawTests extends BaseSuite {
  locally {
    import LiftScopeTests.arbitraryFunctionKListList

    checkAll("LiftScope[List, List]", LiftScopeTests[List, List].liftScope[String])
    checkAll(
      "LiftScope[List, OptionT[List, *]]",
      LiftScopeTests[List, OptionT[List, *]].liftScope[String]
    )
    checkAll(
      "LiftScope[List, EitherT[List, Int, *]]",
      LiftScopeTests[List, EitherT[List, Int, *]].liftScope[String]
    )
    checkAll(
      "LiftScope[List, IorT[List, Int, *]]",
      LiftScopeTests[List, IorT[List, Int, *]].liftScope[String]
    )
    checkAll(
      "LiftScope[List, Kleisli[List, Int, *]]",
      LiftScopeTests[List, Kleisli[List, Int, *]].liftScope[String]
    )
    checkAll(
      "LiftScope[List, WriterT[List, Int, *]]",
      LiftScopeTests[List, WriterT[List, Int, *]].liftScope[String]
    )
    checkAll(
      "LiftScope[List, OptionT[IorT[List, Int, *], *]]",
      LiftScopeTests[List, OptionT[IorT[List, Int, *], *]].liftScope[String]
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
                case other => other
              }
          }
        }
      }

    checkAll(
      "LiftScope[List, StateT[List, String, *]]",
      LiftScopeTests[List, StateT[List, String, *]].liftScope[Int]
    )
    checkAll(
      "LiftScope[List, RWST[List, String, String, String, *]]",
      LiftScopeTests[List, RWST[List, String, String, String, *]].liftScope[Int]
    )
  }
}
