package lgbt.princess.lifts
package instances

import cats.data.EitherT
import cats.{Eq, Functor}
import cats.effect.IO
import cats.effect.kernel.{MonadCancelThrow, Resource}
import lgbt.princess.lifts.laws.Unlift
import lgbt.princess.lifts.laws.Unlift.Result
import munit.{CatsEffectSuite, DisciplineSuite}

abstract class CESuite extends CatsEffectSuite with DisciplineSuite {
  implicit def eqIO[A: Eq]: Eq[IO[A]] =
    Eq.by(_.unsafeRunSync())

  implicit def eqResource[F[_], A](implicit
      F: MonadCancelThrow[F],
      eqFA: Eq[F[A]]
  ): Eq[Resource[F, A]] =
    Eq.by(_.use(F.pure))

  implicit def unliftResource[G[_], F[_]](implicit
      F: MonadCancelThrow[G],
      outer: Unlift[G, F]
  ): Unlift[Resource[G, *], F] =
    outer.compose {
      new Unlift[Resource[G, *], G] {
        def functor: Functor[G] = F
        def unlift[A](value: Resource[G, A]): Result[G, A] =
          EitherT(value.use(a => F.pure(Right(a))))
      }
    }
}
