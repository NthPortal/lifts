package lgbt.princess.lifts
package syntax

import cats.{Applicative, ~>}
import cats.mtl.Local

trait LocalSyntax {
  implicit def toLocalLiftToOps[F[_], E](local: Local[F, E]): LocalSyntax.LocalLiftToOps[F, E] =
    new LocalSyntax.LocalLiftToOps(local)
}

object LocalSyntax extends LocalSyntax {
  private[this] final class LiftedLocal[F[_], G[_], E](
      underlying: Local[F, E]
  )(implicit G: Applicative[G], lk: LiftKind[F, G])
      extends Local[G, E] {
    val applicative: Applicative[G] = G
    def ask[E2 >: E]: G[E2] = lk.liftF(underlying.ask[E2])
    def local[A](ga: G[A])(f: E => E): G[A] =
      lk.limitedMapK(ga) {
        new (F ~> F) {
          def apply[B](fb: F[B]): F[B] = underlying.local(fb)(f)
        }
      }
  }

  final class LocalLiftToOps[F[_], E] private[LocalSyntax] (private val local: Local[F, E])
      extends AnyVal {
    def liftTo[G[_]: Applicative](implicit lk: LiftKind[F, G]): Local[G, E] =
      new LiftedLocal(local)
  }
}
