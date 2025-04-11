package lgbt.princess.lifts
package instances

import cats.effect.kernel.{MonadCancel, Resource}
import cats.~>

trait ResourceInstances extends LowPriorityResourceInstances {
  implicit def liftValueResource[F[_]]: LiftValue[F, Resource[F, *]] =
    new LiftValue[F, Resource[F, *]] {
      def liftF[A](value: F[A]): Resource[F, A] = Resource.eval(value)
      val liftK: F ~> Resource[F, *] = Resource.liftK
    }

  implicit def liftScopeResource[F[_]](implicit
      F: MonadCancel[F, _]
  ): LiftScope[F, Resource[F, *]] =
    new LiftScope[F, Resource[F, *]] {
      def liftScopeApply[A](scope: F ~> F)(value: Resource[F, A]): Resource[F, A] =
        value.mapK(scope)
    }
}

sealed trait LowPriorityResourceInstances {
  implicit def liftKindResource[F[_]](implicit F: MonadCancel[F, _]): LiftKind[F, Resource[F, *]] =
    new LiftKind[F, Resource[F, *]] {
      def liftF[A](value: F[A]): Resource[F, A] = Resource.eval(value)
      val liftK: F ~> Resource[F, *] = Resource.liftK
      def liftScopeApply[A](scope: F ~> F)(value: Resource[F, A]): Resource[F, A] =
        value.mapK(scope)
    }
}

object ResourceInstances extends ResourceInstances
