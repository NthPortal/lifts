package lgbt.princess.lifts

import cats.data._
import lgbt.princess.lifts.laws.discipline.LiftValueTests

class LiftValueLawTests extends BaseSuite {
  checkAll("LiftValue[List, List]", LiftValueTests[List, List].liftValue[String])
  checkAll(
    "LiftValue[List, OptionT[List, *]]",
    LiftValueTests[List, OptionT[List, *]].liftValue[String]
  )
  checkAll(
    "LiftValue[List, EitherT[List, Int, *]]",
    LiftValueTests[List, EitherT[List, Int, *]].liftValue[String]
  )
  checkAll(
    "LiftValue[List, IorT[List, Int, *]]",
    LiftValueTests[List, IorT[List, Int, *]].liftValue[String]
  )
  checkAll(
    "LiftValue[List, Kleisli[List, Int, *]]",
    LiftValueTests[List, Kleisli[List, Int, *]].liftValue[String]
  )
  checkAll(
    "LiftValue[List, StateT[List, Int, *]]",
    LiftValueTests[List, StateT[List, Int, *]].liftValue[String]
  )
  checkAll(
    "LiftValue[List, WriterT[List, Int, *]]",
    LiftValueTests[List, WriterT[List, Int, *]].liftValue[String]
  )
  checkAll(
    "LiftValue[List, OptionT[IorT[List, Int, *], *]]",
    LiftValueTests[List, OptionT[IorT[List, Int, *], *]].liftValue[String]
  )
}
