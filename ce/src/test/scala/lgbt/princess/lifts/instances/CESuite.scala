package lgbt.princess.lifts
package instances

import cats.Eq
import cats.effect.IO
import munit.{CatsEffectSuite, DisciplineSuite}

abstract class CESuite extends CatsEffectSuite with DisciplineSuite {
  implicit def eqIO[A: Eq]: Eq[IO[A]] =
    Eq.by(_.unsafeRunSync())
}
