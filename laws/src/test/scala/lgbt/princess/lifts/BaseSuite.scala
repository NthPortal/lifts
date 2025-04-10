package lgbt.princess.lifts

import cats.data.{Kleisli, StateT}
import cats.laws.discipline.ExhaustiveCheck
import cats.laws.discipline.eq._
import cats.{Eq, FlatMap}
import munit.DisciplineSuite
import org.scalacheck.{Arbitrary, Gen}

abstract class BaseSuite extends DisciplineSuite {
  implicit def notActuallyExhaustiveCheck[A](implicit arb: Arbitrary[A]): ExhaustiveCheck[A] =
    new ExhaustiveCheck[A] {
      def allValues: List[A] =
        Gen
          .listOfN(32, arb.arbitrary)
          .sample
          .getOrElse(throw new RuntimeException("Gen failed!"))
    }

  implicit def eqKleisli[F[_], A, B](implicit
      arb: Arbitrary[A],
      ev: Eq[F[B]]
  ): Eq[Kleisli[F, A, B]] =
    Eq.by(_.run)

  implicit def stateTEq[F[_], S, A](implicit
      S: Arbitrary[S],
      FSA: Eq[F[(S, A)]],
      F: FlatMap[F]
  ): Eq[StateT[F, S, A]] =
    Eq.by[StateT[F, S, A], S => F[(S, A)]](state => s => state.run(s))
}
