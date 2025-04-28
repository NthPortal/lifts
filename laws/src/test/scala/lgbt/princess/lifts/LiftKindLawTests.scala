package lgbt.princess.lifts

import cats.data._
import cats.laws.discipline.arbitrary._
import lgbt.princess.lifts.laws.discipline.LiftKindTests

class LiftKindLawTests extends BaseSuite {
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
