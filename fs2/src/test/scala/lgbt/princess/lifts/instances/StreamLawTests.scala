package lgbt.princess.lifts.instances

import cats.data.OptionT
import cats.effect.kernel.testkit.PureConcGenerators._
import cats.effect.kernel.testkit.pure._
import cats.laws.discipline.arbitrary.catsLawsArbitraryForOptionT
import fs2.Stream
import lgbt.princess.lifts.instances.StreamInstances._
import lgbt.princess.lifts.laws.discipline._

class StreamLawTests extends Fs2Suite {
  checkAll(
    "LiftValue[PCT, Stream[PCT, *]]",
    LiftValueTests[PCT, Stream[PCT, *]].liftValue[Int]
  )
  checkAll(
    "LiftScope[PCT, Stream[PCT, *]]",
    LiftScopeTests[PCT, Stream[PCT, *]].liftScope[Int]
  )
  checkAll(
    "LiftKind[PCT, Stream[PCT, *]]",
    LiftKindTests[PCT, Stream[PCT, *]].liftKind[Int]
  )
  checkAll(
    "MapK[PCT, OptionT[PCT, *], Stream[PCT, *], Stream[OptionT[PCT, *], *]]",
    MapKTests[PCT, OptionT[PCT, *], Stream[PCT, *], Stream[OptionT[PCT, *], *]]
      .mapK[Int]
  )
  checkAll(
    "LiftKind1[PCT, Stream[PCT, *]]",
    LiftKind1Tests[PCT, Stream[PCT, *]].liftKind1[Int]
  )
  checkAll(
    "LiftKind2[PCT, OptionT[PCT, *], Stream[PCT, *], Stream[OptionT[PCT, *], *]]",
    LiftKind2Tests[PCT, OptionT[PCT, *], Stream[PCT, *], Stream[OptionT[PCT, *], *]]
      .liftKind2[Int]
  )

  // TODO: test `LiftValue[Resource[F, *], Stream[F, *]]`
}
