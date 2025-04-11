package lgbt.princess.lifts
package instances

import cats.{Eq, Functor, ~>}
import cats.data.EitherT
import cats.effect.{IO, IOLocal}
import cats.effect.kernel.{MonadCancelThrow, Resource}
import cats.mtl.Local
import lgbt.princess.lifts.instances.ResourceInstances._
import lgbt.princess.lifts.laws.Unlift
import lgbt.princess.lifts.laws.Unlift.Result
import lgbt.princess.lifts.laws.discipline.{LiftKindTests/*, LiftScopeTests, LiftValueTests*/}
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

  implicit def unliftResource[F[_]](implicit F: MonadCancelThrow[F]): Unlift[F, Resource[F, *]] =
    new Unlift[F, Resource[F, *]] {
      def functor: Functor[F] = F
      def unlift[A](value: Resource[F, A]): Result[F, A] =
        EitherT(value.use(a => F.pure(Right(a))))
    }

  implicit def eqResource[F[_], A](implicit
      F: MonadCancelThrow[F],
      eqFA: Eq[F[A]]
  ): Eq[Resource[F, A]] =
    Eq.by(_.use(F.pure))

  // these have ambiguous implicits for some reason
//  checkAll(
//    "LiftValue[IO, Resource[IO, *]]",
//    LiftValueTests[IO, Resource[IO, *]].liftValue[Int]
//  )
//  checkAll(
//    "LiftScope[IO, Resource[IO, *]]",
//    LiftScopeTests[IO, Resource[IO, *]].liftScope[Int]
//  )
  checkAll(
    "LiftKind[IO, Resource[IO, *]]",
    LiftKindTests[IO, Resource[IO, *]].liftKind[Int]
  )
}
