package lgbt.princess.lifts
package instances

import cats.effect.kernel.{MonadCancel, Resource}
import cats.~>

trait ResourceInstances extends LowPriorityResourceInstances {
  implicit def liftValueResource[F[_]]: LiftValue[F, Resource[F, *]] =
    liftValueResource0

  implicit def liftScopeResource[F[_]](implicit
      F: MonadCancel[F, ?]
  ): LiftScope[F, Resource[F, *]] =
    new LiftScope[F, Resource[F, *]] {
      def liftScopeApply[A](scope: F ~> F)(value: Resource[F, A]): Resource[F, A] =
        value.mapK(scope)
    }

  implicit def mapKResource[F[_], G[_]](implicit
      F: MonadCancel[F, ?],
      G: MonadCancel[G, ?]
  ): MapK.Derived[F, G, Resource] =
    new MapK.Derived[F, G, Resource] {
      def mapK[A](value: Resource[F, A])(f: F ~> G): Resource[G, A] =
        value.mapK(f)
    }
}

sealed trait LowPriorityResourceInstances {
  protected[this] def liftValueResource0[F[_]]: LiftValue[F, Resource[F, *]] =
    new LiftValue[F, Resource[F, *]] {
      def liftF[A](value: F[A]): Resource[F, A] = Resource.eval(value)
      val liftK: F ~> Resource[F, *] = Resource.liftK
    }

  implicit def liftKindResource[F[_]](implicit F: MonadCancel[F, ?]): LiftKind[F, Resource[F, *]] =
    new LiftKind[F, Resource[F, *]] {
      def liftF[A](value: F[A]): Resource[F, A] = Resource.eval(value)
      val liftK: F ~> Resource[F, *] = Resource.liftK
      def liftScopeApply[A](scope: F ~> F)(value: Resource[F, A]): Resource[F, A] =
        value.mapK(scope)
    }

  implicit def liftKind1Resource[F[_]](implicit
      F: MonadCancel[F, ?]
  ): LiftKind1[F, Resource[F, *]] =
    new LiftKind1[F, Resource[F, *]] {
      def liftF[A](value: F[A]): Resource[F, A] = Resource.eval(value)
      val liftK: F ~> Resource[F, *] = Resource.liftK
      def mapK[A](value: Resource[F, A])(f: F ~> F): Resource[F, A] =
        value.mapK(f)
    }

  implicit def liftKind2Resource[F[_], G[_]](implicit
      F: MonadCancel[F, ?],
      G: MonadCancel[G, ?]
  ): LiftKind2.Derived[F, G, Resource] =
    new LiftKind2.Derived[F, G, Resource] {
      val liftValue1: LiftValue[F, Resource[F, *]] = liftValueResource0
      val liftValue2: LiftValue[G, Resource[G, *]] = liftValueResource0
      def mapK[A](value: Resource[F, A])(f: F ~> G): Resource[G, A] =
        value.mapK(f)
    }
}

object ResourceInstances extends ResourceInstances
