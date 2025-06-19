package lgbt.princess.lifts
package instances

import cats.effect.{IO, LiftIO}

trait LiftIOInstances {
  implicit def liftValueLiftIO[To[_]](implicit liftIO: LiftIO[To]): LiftValue[IO, To] =
    new LiftValue[IO, To] {
      def liftF[A](value: IO[A]): To[A] = liftIO.liftIO(value)
    }
}

object LiftIOInstances extends LiftIOInstances
