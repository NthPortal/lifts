package lgbt.princess.lifts

import cats.data._
import cats.laws.discipline.arbitrary._
import lgbt.princess.lifts.laws.discipline.LiftKind1Tests

class LiftKind1LawTests extends BaseSuite {
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
