package lgbt.princess.lifts

import cats.Eq
import cats.data.Kleisli
import cats.laws.discipline.ExhaustiveCheck
import cats.laws.discipline.eq._
import munit.DisciplineSuite
import org.scalacheck.{Arbitrary, Gen}

abstract class BaseSuite extends DisciplineSuite {
  implicit def notActuallyExhaustiveCheck[A](implicit arb: Arbitrary[A]): ExhaustiveCheck[A] =
    new ExhaustiveCheck[A] {
      def allValues: List[A] =
        Gen
          .listOfN(16, arb.arbitrary)
          .sample
          .getOrElse(throw new RuntimeException("Gen failed!"))
    }

  implicit def eqKleisli[F[_], A, B](implicit
      arbA: Arbitrary[A],
      eqFB: Eq[F[B]]
  ): Eq[Kleisli[F, A, B]] =
    Eq.by(_.run)
}
