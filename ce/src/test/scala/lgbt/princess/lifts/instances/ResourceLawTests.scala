package lgbt.princess.lifts
package instances

import cats.Functor
import cats.data.EitherT
import cats.effect.kernel.testkit.pure.PureConc
import cats.effect.kernel.{MonadCancelThrow, Resource}
import lgbt.princess.lifts.laws.Unlift
import lgbt.princess.lifts.laws.Unlift.Result
//import lgbt.princess.lifts.laws.discipline.LiftValueTests

class ResourceLawTests extends BaseSuite {
  implicit def unliftResource[F[_]](implicit F: MonadCancelThrow[F]): Unlift[F, Resource[F, *]] =
    new Unlift[F, Resource[F, *]] {
      def functor: Functor[F] = F
      def unlift[A](value: Resource[F, A]): Result[F, A] =
        EitherT(value.use(a => F.pure(Right(a))))
    }

//  checkAll(
//    "LiftValue[F, Resource[F, *]]",
//    LiftValueTests[PureConc[Int, *], Resource[PureConc[Int, *], *]].liftValue[String]
//  )
}
