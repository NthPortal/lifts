package lgbt.princess.lifts

import cats.{Eq, FlatMap, Monad}
import cats.data._
import cats.laws.discipline.eq._
import lgbt.princess.lifts.laws.discipline.LiftValueTests
import org.scalacheck.Arbitrary

class LiftValueLawTests extends BaseSuite {
  implicit def stateTEq[F[_]: FlatMap, S, A](implicit
      arbS: Arbitrary[S],
      eqFSA: Eq[F[(S, A)]],
  ): Eq[StateT[F, S, A]] =
    Eq.by(state => (s: S) => state.run(s))

  implicit def rwstEq[F[_]: Monad, E, L, S, A](implicit
      arbE: Arbitrary[E],
      arbS: Arbitrary[S],
      eqFLSA: Eq[F[(L, S, A)]]
  ): Eq[RWST[F, E, L, S, A]] =
    Eq.by(rwst => (e: E) => (s: S) => rwst.run(e, s))

  checkAll("LiftValue[List, List]", LiftValueTests[List, List].liftValue[String])
  checkAll(
    "LiftValue[List, OptionT[List, *]]",
    LiftValueTests[List, OptionT[List, *]].liftValue[String]
  )
  checkAll(
    "LiftValue[List, EitherT[List, Int, *]]",
    LiftValueTests[List, EitherT[List, Int, *]].liftValue[String]
  )
  checkAll(
    "LiftValue[List, IorT[List, Int, *]]",
    LiftValueTests[List, IorT[List, Int, *]].liftValue[String]
  )
  checkAll(
    "LiftValue[List, Kleisli[List, Int, *]]",
    LiftValueTests[List, Kleisli[List, Int, *]].liftValue[String]
  )
  checkAll(
    "LiftValue[List, StateT[List, Int, *]]",
    LiftValueTests[List, StateT[List, Int, *]].liftValue[String]
  )
  checkAll(
    "LiftValue[List, WriterT[List, Int, *]]",
    LiftValueTests[List, WriterT[List, Int, *]].liftValue[String]
  )
  checkAll(
    "LiftValue[List, RWST[List, Int, Int, Int, *]]",
    LiftValueTests[List, RWST[List, Int, Int, Int, *]].liftValue[String]
  )
  checkAll(
    "LiftValue[List, OptionT[IorT[List, Int, *], *]]",
    LiftValueTests[List, OptionT[IorT[List, Int, *], *]].liftValue[String]
  )
}
