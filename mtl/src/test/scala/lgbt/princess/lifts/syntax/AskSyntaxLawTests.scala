package lgbt.princess.lifts
package syntax

import cats.data.OptionT
import cats.effect.IO
import cats.laws.discipline.arbitrary._
import cats.mtl.Ask
import cats.mtl.laws.discipline.AskTests
import lgbt.princess.lifts.syntax.mtl._
import org.scalacheck.Arbitrary

class AskSyntaxLawTests extends MtlSuite {
  implicit val askIO: Ask[IO, Int] =
    Ask.const(16)
  implicit val askOptionT: Ask[OptionT[IO, *], Int] =
    askIO.liftTo[OptionT[IO, *]]

  implicit def arbitraryIO[A](implicit arb: Arbitrary[A]): Arbitrary[IO[A]] =
    Arbitrary(for (value <- arb.arbitrary) yield IO(value))

  checkAll("Ask[IO, Int]", AskTests[IO, Int].ask[String])
  checkAll("Ask[OptionT[IO, *], Int]", AskTests[OptionT[IO, *], Int].ask[String])
}
