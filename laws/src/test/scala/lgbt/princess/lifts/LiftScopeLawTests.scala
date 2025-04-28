package lgbt.princess.lifts

import cats.data._
import cats.laws.discipline.arbitrary._
import lgbt.princess.lifts.laws.discipline.LiftScopeTests

class LiftScopeLawTests extends BaseSuite {
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
