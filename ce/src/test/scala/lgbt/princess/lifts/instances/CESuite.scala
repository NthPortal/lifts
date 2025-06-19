package lgbt.princess.lifts
package instances

import cats.data.{EitherT, OptionT}
import cats.effect.kernel.testkit.pure._
import cats.effect.{Concurrent, MonadCancelThrow, Ref, Resource}
import cats.syntax.flatMap._
import cats.{Eq, Functor, ~>}
import lgbt.princess.lifts.laws.Unlift
import munit.DisciplineSuite
import org.scalacheck.{Arbitrary, Gen}

trait CESuite extends DisciplineSuite {
  type PCT[A] = PureConc[Throwable, A]

  private[this] val counter: PCT[Ref[PCT, Int]] = Concurrent[PCT].ref(0)

  implicit val eqThrowable: Eq[Throwable] = Eq.fromUniversalEquals

  implicit val arbitraryScope: Arbitrary[PCT ~> PCT] =
    Arbitrary {
      Gen.const {
        new (PCT ~> PCT) {
          def apply[A](fa: PCT[A]): PCT[A] =
            for {
              ref <- counter
              res <- ref.update(_ + 1) >> fa
            } yield res
        }
      }
    }

  implicit val arbitraryOptionTScope: Arbitrary[PCT ~> OptionT[PCT, *]] =
    Arbitrary {
      arbitraryScope.arbitrary.map {
        _.andThen {
          new (PCT ~> OptionT[PCT, *]) {
            def apply[A](fa: PCT[A]): OptionT[PCT, A] = OptionT.liftF(fa)
          }
        }
      }
    }

  implicit def unliftResource[From[_], To[_]](implicit
      From: MonadCancelThrow[From],
      outer: Unlift[From, To]
  ): Unlift[Resource[From, *], To] =
    outer.compose {
      new Unlift[Resource[From, *], From] {
        def functor: Functor[From] = From
        def unlift[A](value: Resource[From, A]): Unlift.Result[From, A] =
          EitherT(value.use(a => From.pure(Right(a))))
      }
    }

  // TODO: use `Resource#allocated` instead of `#use`
  implicit def eqResource[F[_], A](implicit
      F: MonadCancelThrow[F],
      eqFA: Eq[F[A]]
  ): Eq[Resource[F, A]] =
    Eq.by(_.use(F.pure))

  // TODO: add arbitrary release effect
  implicit def arbitraryResource[F[_], A](implicit
      arbFA: Arbitrary[F[A]]
  ): Arbitrary[Resource[F, A]] =
    Arbitrary(arbFA.arbitrary.map(Resource.eval))
}
