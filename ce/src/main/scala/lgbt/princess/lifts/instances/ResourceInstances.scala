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

  implicit def liftValueResource[F[_], G[_]](implicit
      inner: LiftValue[F, G]
  ): LiftValue[F, Resource[G, *]] =
    inner.andThen(liftValueResource0[G])

  implicit def liftKindResource[F[_], G[_]](implicit
      G: MonadCancel[G, ?],
      inner: LiftKind[F, G]
  ): LiftKind[F, Resource[G, *]] =
    inner.andThen {
      new LiftKind[G, Resource[G, *]] {
        def liftF[A](value: G[A]): Resource[G, A] = Resource.eval(value)
        val liftK: G ~> Resource[G, *] = Resource.liftK
        def liftScopeApply[A](scope: G ~> G)(value: Resource[G, A]): Resource[G, A] =
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
        val liftValue1: LiftValue[H, Resource[H, *]] = liftValueResource0
        val liftValue2: LiftValue[I, Resource[I, *]] = liftValueResource0
        def mapK[A](value: Resource[H, A])(f: H ~> I): Resource[I, A] =
          value.mapK(f)
      }
    }
}

sealed trait LowPriorityResourceInstances {
  implicit def liftKind1Resource[F[_], G[_]](implicit
      F: MonadCancel[G, ?],
      inner: LiftKind1[F, G]
  ): LiftKind1[F, Resource[G, *]] =
    inner.andThen {
      new LiftKind1[G, Resource[G, *]] {
        def liftF[A](value: G[A]): Resource[G, A] = Resource.eval(value)
        val liftK: G ~> Resource[G, *] = Resource.liftK
        def mapK[A](value: Resource[G, A])(f: G ~> G): Resource[G, A] =
          value.mapK(f)
      }
    }
}

object ResourceInstances extends ResourceInstances
