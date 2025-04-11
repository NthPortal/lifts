package lgbt.princess.lifts
package instances

import cats.{Eq, Functor}
import cats.data.EitherT
import cats.effect.{IO, LiftIO}
import lgbt.princess.lifts.instances.ce._
import lgbt.princess.lifts.laws.Unlift
import lgbt.princess.lifts.laws.Unlift.Result
import lgbt.princess.lifts.laws.discipline.LiftValueTests
import org.scalacheck.Arbitrary

class LiftIOLawTests extends CESuite {
  // evidence only for compile-time proof
  implicit def unliftIO[F[_]: LiftIO]: Unlift[IO, F] =
    new Unlift[IO, F] {
      def functor: Functor[IO] = implicitly
      def unlift[A](value: F[A]): Result[IO, A] =
        value match {
          case io: IO[A @unchecked] => Unlift.success(io)
          case other =>
            EitherT.left {
              IO {
                Unlift.Failure {
                  s"value of type ${other.getClass.getName} is not IO. value was: $other"
                }
              }
            }
        }
    }

  implicit def arbitraryIO[A](implicit arb: Arbitrary[A]): Arbitrary[IO[A]] =
    Arbitrary(for (value <- arb.arbitrary) yield IO.pure(value))

  def run[F[_]: LiftIO]()(implicit eqFStr: Eq[F[String]]): Unit = {
    checkAll("LiftValue[IO, F]", LiftValueTests[IO, F].liftValue[String])
  }
  run[IO]()
}
