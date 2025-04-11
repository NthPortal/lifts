package lgbt.princess.lifts.syntax

import cats.Eq
import cats.effect.IO
import munit.{CatsEffectSuite, DisciplineSuite}

abstract class MtlSuite extends CatsEffectSuite with DisciplineSuite {
  implicit def eqIO[A: Eq]: Eq[IO[A]] =
    Eq.by(_.unsafeRunSync())
}
