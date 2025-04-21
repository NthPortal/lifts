package lgbt.princess.lifts
package instances

import cats.effect.kernel.Resource
import cats.effect.{IO, IOLocal}
import cats.mtl.Local
import cats.~>
import lgbt.princess.lifts.instances.ResourceInstances._
import lgbt.princess.lifts.laws.discipline.{LiftKindTests, LiftScopeTests, LiftValueTests}
import lgbt.princess.lifts.syntax.mtl._
import org.scalacheck.{Arbitrary, Gen}

class ResourceLawTests extends CESuite {
  implicit val counter: Local[IO, Int] =
    IOLocal(0).unsafeRunSync().asLocal

  implicit val arbitraryIOInt: Arbitrary[IO[Int]] =
    Arbitrary(Gen.const(counter.ask[Int]))
  implicit val arbitraryResourceIOInt: Arbitrary[Resource[IO, Int]] =
    Arbitrary(Gen.const(counter.liftTo[Resource[IO, *]].ask[Int]))

  implicit val arbitraryIOIO: Arbitrary[IO ~> IO] =
    Arbitrary {
      Gen.const {
        new (IO ~> IO) {
          def apply[A](fa: IO[A]): IO[A] =
            counter.local(fa)(_ + 1)
        }
      }
    }

  checkAll(
    "LiftValue[IO, Resource[IO, *]]",
    LiftValueTests[IO, Resource[IO, *]].liftValue[Int]
  )
  checkAll(
    "LiftScope[IO, Resource[IO, *]]",
    LiftScopeTests[IO, Resource[IO, *]].liftScope[Int]
  )
  checkAll(
    "LiftKind[IO, Resource[IO, *]]",
    LiftKindTests[IO, Resource[IO, *]].liftKind[Int]
  )
}
