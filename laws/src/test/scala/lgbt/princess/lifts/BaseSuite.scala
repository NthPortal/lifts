package lgbt.princess.lifts

import cats.data.{Kleisli, RWST, StateT}
import cats.laws.discipline.ExhaustiveCheck
import cats.laws.discipline.eq._
import cats.{Eq, FlatMap, Monad}
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

  implicit def stateTEq[F[_]: FlatMap, S, A](implicit
      arbS: Arbitrary[S],
      eqFSA: Eq[F[(S, A)]],
  ): Eq[StateT[F, S, A]] =
    Eq.by[StateT[F, S, A], S => F[(S, A)]](state => s => state.run(s))

  implicit def rwstEq[F[_]: Monad, E, L, S, A](implicit
      arbE: Arbitrary[E],
      arbS: Arbitrary[S],
      eqFLSA: Eq[F[(L, S, A)]]
  ): Eq[RWST[F, E, L, S, A]] =
    Eq.by[RWST[F, E, L, S, A], E => S => F[(L, S, A)]](state => e => s => state.run(e, s))
}
