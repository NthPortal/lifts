package lgbt.princess.lifts.instances

import cats.data.EitherT
import cats.effect.Concurrent
import cats.syntax.functor._
import cats.{Eq, Functor}
import fs2.Stream
import lgbt.princess.lifts.laws.Unlift
import org.scalacheck.Arbitrary

abstract class Fs2Suite extends CESuite {
  implicit def unliftStream[From[_], To[_]](implicit
      From: Concurrent[From],
      outer: Unlift[From, To]
  ): Unlift[Stream[From, *], To] =
    outer.compose {
      new Unlift[Stream[From, *], From] {
        def functor: Functor[From] = From
        def unlift[A](value: Stream[From, A]): Unlift.Result[From, A] = {
          EitherT {
            value.compile.toVector
              .map {
                case Seq() => Left(Unlift.Failure("fs2.Stream.empty"))
                case Seq(single) => Right(single)
                case seq => Left(Unlift.Failure(s"fs2.Stream(<count: ${seq.length}>)"))
              }
          }
        }
      }
    }

  implicit def eqStream[F[_], A](implicit
      F: Concurrent[F],
      eqFA: Eq[F[Vector[A]]]
  ): Eq[Stream[F, A]] =
    Eq.by(_.compile.toVector)

  implicit def arbitraryStream[F[_], A](implicit
      arbFA: Arbitrary[F[A]]
  ): Arbitrary[Stream[F, A]] =
    Arbitrary(arbFA.arbitrary.map(Stream.eval))
}
