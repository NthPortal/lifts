package lgbt.princess.lifts
package instances

import cats.effect.{IO, LiftIO}

trait LiftIOInstances {
  implicit def liftValueLiftIO[F[_]](implicit liftIO: LiftIO[F]): LiftValue[IO, F] =
    new LiftValue.LiftKFromLiftF[IO, F] {
      def liftF[A](value: IO[A]): F[A] = liftIO.liftIO(value)
    }
}

object LiftIOInstances extends LiftIOInstances
