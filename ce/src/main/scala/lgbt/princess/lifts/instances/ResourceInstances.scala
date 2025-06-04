package lgbt.princess.lifts
package instances

import cats.effect.kernel.{MonadCancel, Resource}
import cats.~>

trait ResourceInstances extends LowPriorityResourceInstances {
  private[this] def liftValueResource0[F[_]]: LiftValue[F, Resource[F, *]] =
    new LiftValue[F, Resource[F, *]] {
      def liftF[A](value: F[A]): Resource[F, A] = Resource.eval(value)
      val liftK: F ~> Resource[F, *] = Resource.liftK
    }

  implicit def liftValueResource[From[_], To[_]](implicit
      inner: LiftValue[From, To]
  ): LiftValue[From, Resource[To, *]] =
    inner.andThen(liftValueResource0[To])

  implicit def liftKindResource[From[_], To[_]](implicit
      G: MonadCancel[To, ?],
      inner: LiftKind[From, To]
  ): LiftKind[From, Resource[To, *]] =
    inner.andThen {
      new LiftKind[To, Resource[To, *]] {
        def liftF[A](value: To[A]): Resource[To, A] = Resource.eval(value)
        val liftK: To ~> Resource[To, *] = Resource.liftK
        def limitedMapK[A](value: Resource[To, A])(scope: To ~> To): Resource[To, A] =
          value.mapK(scope)
      }
    }

  implicit def liftKind2Resource[F[_], G[_], H[_], I[_]](implicit
      H: MonadCancel[H, ?],
      I: MonadCancel[I, ?],
      inner: LiftKind2[F, G, H, I]
  ): LiftKind2[F, G, Resource[H, *], Resource[I, *]] =
    inner.andThen {
      new LiftKind2[H, I, Resource[H, *], Resource[I, *]] {
        val liftValueInput: LiftValue[H, Resource[H, *]] = liftValueResource0
        val liftValueOutput: LiftValue[I, Resource[I, *]] = liftValueResource0
        def mapK[A](value: Resource[H, A])(f: H ~> I): Resource[I, A] =
          value.mapK(f)
      }
    }
}

sealed trait LowPriorityResourceInstances {
  implicit def liftKind1Resource[From[_], To[_]](implicit
      F: MonadCancel[To, ?],
      inner: LiftKind1[From, To]
  ): LiftKind1[From, Resource[To, *]] =
    inner.andThen {
      new LiftKind1[To, Resource[To, *]] {
        def liftF[A](value: To[A]): Resource[To, A] = Resource.eval(value)
        val liftK: To ~> Resource[To, *] = Resource.liftK
        def mapK[A](value: Resource[To, A])(f: To ~> To): Resource[To, A] =
          value.mapK(f)
      }
    }
}

object ResourceInstances extends ResourceInstances
