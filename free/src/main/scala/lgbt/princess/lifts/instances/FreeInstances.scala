package lgbt.princess.lifts
package instances

import cats.free.Free
import cats.~>

trait FreeInstances extends LowPriorityFreeInstances {
  private[this] def liftValueFree0[F[_]]: LiftValue[F, Free[F, *]] =
    new LiftValue[F, Free[F, *]] {
      def liftF[A](value: F[A]): Free[F, A] = Free.liftF(value)
    }

  implicit def liftValueFree[From[_], To[_]](implicit
      inner: LiftValue[From, To]
  ): LiftValue[From, Free[To, *]] =
    inner.andThen(liftValueFree0)

  implicit def liftKindFree[From[_], To[_]](implicit
      inner: LiftKind[From, To]
  ): LiftKind[From, Free[To, *]] =
    inner.andThen {
      new LiftKind[To, Free[To, *]] {
        def liftF[A](value: To[A]): Free[To, A] = Free.liftF(value)
        def limitedMapK[A](value: Free[To, A])(scope: To ~> To): Free[To, A] =
          value.mapK(scope)
      }
    }

  implicit def liftKind2Free[In1[_], Out1[_], In2[_], Out2[_]](implicit
      inner: LiftKind2[In1, Out1, In2, Out2]
  ): LiftKind2[In1, Out1, Free[In2, *], Free[Out2, *]] =
    inner.andThen {
      new LiftKind2[In2, Out2, Free[In2, *], Free[Out2, *]] {
        val liftValueInput: LiftValue[In2, Free[In2, *]] = liftValueFree0
        val liftValueOutput: LiftValue[Out2, Free[Out2, *]] = liftValueFree0
        def mapK[A](value: Free[In2, A])(f: In2 ~> Out2): Free[Out2, A] =
          value.mapK(f)
      }
    }
}

sealed trait LowPriorityFreeInstances {
  implicit def liftKind1Free[From[_], To[_]](implicit
      inner: LiftKind1[From, To]
  ): LiftKind1[From, Free[To, *]] =
    inner.andThen {
      new LiftKind1[To, Free[To, *]] {
        def liftF[A](value: To[A]): Free[To, A] = Free.liftF(value)
        def mapK[A](value: Free[To, A])(f: To ~> To): Free[To, A] =
          value.mapK(f)
      }
    }
}

object FreeInstances extends FreeInstances
