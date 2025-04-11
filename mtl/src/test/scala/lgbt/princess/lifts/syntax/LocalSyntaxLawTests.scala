package lgbt.princess.lifts.syntax

import cats.data.OptionT
import cats.effect.{IO, IOLocal}
import cats.mtl.Local
import cats.mtl.laws.discipline.LocalTests
import lgbt.princess.lifts.syntax.mtl._
import org.scalacheck.{Arbitrary, Gen}

class LocalSyntaxLawTests extends MtlSuite {
  private implicit val counterIO: Local[IO, Int] =
    IOLocal(0).unsafeRunSync().asLocal
  private implicit val counterOptionT: Local[OptionT[IO, *], Int] =
    counterIO.liftTo[OptionT[IO, *]]

  implicit val arbitraryIOInt: Arbitrary[IO[Int]] =
    Arbitrary(Gen.const(counterIO.ask[Int]))
  implicit val arbitraryOptionTIOInt: Arbitrary[OptionT[IO, Int]] =
    Arbitrary(Gen.const(counterOptionT.ask[Int]))

  private val genIntInt: Gen[Int => Int] =
    for (inc <- Gen.oneOf(1 to 5)) yield (_: Int) + inc
  implicit val arbitraryIOIntInt: Arbitrary[IO[Int => Int]] =
    Arbitrary(genIntInt.map(IO.pure))
  implicit val arbitraryOptionTIOIntInt: Arbitrary[OptionT[IO, Int => Int]] =
    Arbitrary(genIntInt.map(OptionT.pure[IO](_)))

  checkAll("Local[IO, Int]", LocalTests[IO, Int].local[Int, Int])
  checkAll("Local[OptionT[IO, *], Int]", LocalTests[OptionT[IO, *], Int].local[Int, Int])
}
