package lgbt.princess.lifts
package instances

import cats.data.EitherT
import cats.effect.{MonadCancelThrow, Resource}
import cats.{Eq, Functor}
import lgbt.princess.lifts.laws.Unlift
import lgbt.princess.lifts.laws.Unlift.Result
import munit.DisciplineSuite

trait CESuite extends DisciplineSuite {
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
