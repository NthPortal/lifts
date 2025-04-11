package lgbt.princess.lifts

import cats.data.{EitherT, OptionT}
import cats.~>

trait LiftFunctionK[F[_], G[_], H[_[_], _]] {
  def mapK[A](value: H[F, A])(f: F ~> G): H[G, A]

  def liftFunctionK(f: F ~> G): H[F, *] ~> H[G, *] =
    new (H[F, *] ~> H[G, *]) {
      def apply[A](fa: H[F, A]): H[G, A] = mapK(fa)(f)
    }
}

object LiftFunctionK {
  type IdT[F[_], A] = F[A]

  implicit def id[F[_], G[_]]: LiftFunctionK[F, G, IdT] =
    new LiftFunctionK[F, G, IdT] {
      def mapK[A](value: IdT[F, A])(f: F ~> G): IdT[G, A] = f(value)
      override def liftFunctionK(f: F ~> G): IdT[F, *] ~> IdT[G, *] =
        f.asInstanceOf[IdT[F, *] ~> IdT[G, *]]
    }

  implicit def optionT[F[_], G[_]]: LiftFunctionK[F, G, OptionT] =
    new LiftFunctionK[F, G, OptionT] {
      def mapK[A](value: OptionT[F, A])(f: F ~> G): OptionT[G, A] =
        value.mapK(f)
    }
}
