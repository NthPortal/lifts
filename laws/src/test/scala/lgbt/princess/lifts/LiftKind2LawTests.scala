package lgbt.princess.lifts

import cats.data._
import cats.laws.discipline.arbitrary._
import lgbt.princess.lifts.laws.discipline.LiftKind2Tests

class LiftKind2LawTests extends BaseSuite {
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
