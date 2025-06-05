package lgbt.princess.lifts.instances

import cats.data.OptionT
import cats.effect._
import cats.effect.kernel.testkit.PureConcGenerators._
import cats.effect.kernel.testkit.pure._
import cats.laws.discipline.arbitrary.catsLawsArbitraryForOptionT
import lgbt.princess.lifts.instances.ResourceInstances._
import lgbt.princess.lifts.laws.discipline._

class ResourceLawTests extends CESuite {
  checkAll(
    "LiftValue[PCT, Resource[PCT, *]]",
    LiftValueTests[PCT, Resource[PCT, *]].liftValue[Int]
  )
  checkAll(
    "LiftScope[PCT, Resource[PCT, *]]",
    LiftScopeTests[PCT, Resource[PCT, *]].liftScope[Int]
  )
  checkAll(
    "LiftKind[PCT, Resource[PCT, *]]",
    LiftKindTests[PCT, Resource[PCT, *]].liftKind[Int]
  )
  checkAll(
    "MapK[PCT, OptionT[PCT, *], Resource[PCT, *], Resource[OptionT[PCT, *], *]]",
    MapKTests[PCT, OptionT[PCT, *], Resource[PCT, *], Resource[OptionT[PCT, *], *]]
      .mapK[Int]
  )
  checkAll(
    "LiftKind1[PCT, Resource[PCT, *]]",
    LiftKind1Tests[PCT, Resource[PCT, *]].liftKind1[Int]
  )
  checkAll(
    "LiftKind2[PCT, OptionT[PCT, *], Resource[PCT, *], Resource[OptionT[PCT, *], *]]",
    LiftKind2Tests[PCT, OptionT[PCT, *], Resource[PCT, *], Resource[OptionT[PCT, *], *]]
      .liftKind2[Int]
  )
}
