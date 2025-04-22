package lgbt.princess.lifts
package instances

import cats.data.OptionT
import cats.effect.kernel.Resource
import cats.effect.{IO, IOLocal, LiftIO}
import cats.mtl.Local
import cats.~>
import lgbt.princess.lifts.instances.ResourceInstances._
import lgbt.princess.lifts.laws.discipline._
import org.scalacheck.{Arbitrary, Gen}

class ResourceLawTests extends CESuite {
  implicit val counter: Local[IO, Int] =
    IOLocal(0).unsafeRunSync().asLocal

  implicit def arbitraryIOLiftedInt[F[_]](implicit liftIO: LiftIO[F]): Arbitrary[F[Int]] =
    Arbitrary(Gen.const(liftIO.liftIO(counter.ask[Int])))

  implicit def arbitraryIOLift[F[_]](implicit liftIO: LiftIO[F]): Arbitrary[IO ~> F] =
    Arbitrary {
      Gen.const {
        new (IO ~> F) {
          def apply[A](fa: IO[A]): F[A] =
            liftIO.liftIO(counter.local(fa)(_ + 1))
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
  checkAll(
    "MapK[IO, OptionT[IO, *], Resource[IO, *], Resource[OptionT[IO, *], *]]",
    MapKTests[IO, OptionT[IO, *], Resource[IO, *], Resource[OptionT[IO, *], *]].mapK[Int]
  )
  checkAll(
    "LiftKind1[IO, Resource[IO, *]]",
    LiftKind1Tests[IO, Resource[IO, *]].liftKind1[Int]
  )
  checkAll(
    "LiftKind2[IO, OptionT[IO, *], Resource[IO, *], Resource[OptionT[IO, *], *]]",
    LiftKind2Tests[IO, OptionT[IO, *], Resource[IO, *], Resource[OptionT[IO, *], *]].liftKind2[Int]
  )
}
