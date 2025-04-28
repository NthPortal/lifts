package lgbt.princess.lifts

import cats.data._
import cats.laws.discipline.arbitrary._
import lgbt.princess.lifts.laws.discipline.MapKTests

class MapKLawTests extends BaseSuite {
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
