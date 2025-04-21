package lgbt.princess.lifts
package instances

import cats.Eq
import cats.data.OptionT
import cats.effect.{IO, LiftIO, Resource}
import lgbt.princess.lifts.instances.ce._
import lgbt.princess.lifts.laws.Unlift
import lgbt.princess.lifts.laws.discipline.LiftValueTests
import org.scalacheck.Arbitrary

class LiftIOLawTests extends CESuite {
  implicit def arbitraryIO[A](implicit arb: Arbitrary[A]): Arbitrary[IO[A]] =
    Arbitrary(for (value <- arb.arbitrary) yield IO.pure(value))

  def test[F[_]: LiftIO](typeName: String)(implicit eqFStr: Eq[F[String]], unlift: Unlift[F, IO]): Unit = {
    checkAll(s"LiftValue[IO, $typeName]", LiftValueTests[IO, F].liftValue[String])
  }
  test[IO]("IO")
  test[OptionT[IO, *]]("OptionT[IO, *]")
  test[Resource[IO, *]]("Resource[IO, *]")
  test[OptionT[Resource[IO, *], *]]("Resource[OptionT[IO, *], *]")
  test[Resource[OptionT[IO, *], *]]("Resource[OptionT[IO, *], *]")
}
