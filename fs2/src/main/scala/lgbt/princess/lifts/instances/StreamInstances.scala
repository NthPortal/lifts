package lgbt.princess.lifts
package instances

import cats.effect.{MonadCancel, Resource}
import cats.~>
import fs2.Stream

trait StreamInstances extends LowPriorityStreamInstances {
  private[this] def liftValueStream0[F[_]]: LiftValue[F, Stream[F, *]] =
    new LiftValue.LiftKFromLiftF[F, Stream[F, *]] {
      def liftF[A](value: F[A]): Stream[F, A] =
        Stream.eval(value)
    }

  implicit def liftValueStream[From[_], To[_]](implicit
      inner: LiftValue[From, To]
  ): LiftValue[From, Stream[To, *]] =
    inner.andThen(liftValueStream0)

  implicit def liftKindStream[From[_], To[_]](implicit
      inner: LiftKind[From, To]
  ): LiftKind[From, Stream[To, *]] =
    inner.andThen {
      new LiftKind[To, Stream[To, *]] with LiftValue.LiftKFromLiftF[To, Stream[To, *]] {
        def liftF[A](value: To[A]): Stream[To, A] = Stream.eval(value)
        def limitedMapK[A](value: Stream[To, A])(scope: To ~> To): Stream[To, A] =
          value.translate(scope)
      }
    }

  implicit def liftKind2Stream[In1[_], Out1[_], In2[_], Out2[_]](implicit
      inner: LiftKind2[In1, Out1, In2, Out2]
  ): LiftKind2[In1, Out1, Stream[In2, *], Stream[Out2, *]] =
    inner.andThen {
      new LiftKind2[In2, Out2, Stream[In2, *], Stream[Out2, *]] {
        val liftValueInput: LiftValue[In2, Stream[In2, *]] = liftValueStream0
        val liftValueOutput: LiftValue[Out2, Stream[Out2, *]] = liftValueStream0
        def mapK[A](value: Stream[In2, A])(f: In2 ~> Out2): Stream[Out2, A] =
          value.translate(f)
      }
    }
}

sealed trait LowPriorityStreamInstances {
  implicit def liftValueStreamFromResource[From[_], To[_]](implicit
      To: MonadCancel[To, ?],
      inner: LiftValue[From, Resource[To, *]]
  ): LiftValue[From, Stream[To, *]] =
    inner.andThen {
      new LiftValue.LiftKFromLiftF[Resource[To, *], Stream[To, *]] {
        def liftF[A](value: Resource[To, A]): Stream[To, A] =
          Stream.resource(value)
      }
    }

  implicit def liftKind1Stream[From[_], To[_]](implicit
      inner: LiftKind1[From, To]
  ): LiftKind1[From, Stream[To, *]] =
    inner.andThen {
      new LiftKind1[To, Stream[To, *]] with LiftValue.LiftKFromLiftF[To, Stream[To, *]] {
        def liftF[A](value: To[A]): Stream[To, A] = Stream.eval(value)
        def mapK[A](value: Stream[To, A])(f: To ~> To): Stream[To, A] =
          value.translate(f)
      }
    }
}

object StreamInstances extends StreamInstances
