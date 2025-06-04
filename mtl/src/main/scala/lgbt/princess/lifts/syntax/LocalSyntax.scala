package lgbt.princess.lifts
package syntax

import cats.{Applicative, ~>}
import cats.mtl.Local

trait LocalSyntax {
  implicit def toLocalLiftToOps[From[_], E](
      local: Local[From, E]
  ): LocalSyntax.LocalLiftToOps[From, E] =
    new LocalSyntax.LocalLiftToOps(local)
}

object LocalSyntax extends LocalSyntax {
  private[this] final class LiftedLocal[From[_], To[_], E](
      underlying: Local[From, E]
  )(implicit G: Applicative[To], lk: LiftKind[From, To])
      extends Local[To, E] {
    val applicative: Applicative[To] = G
    def ask[E2 >: E]: To[E2] = lk.liftF(underlying.ask[E2])
    def local[A](ga: To[A])(f: E => E): To[A] =
      lk.limitedMapK(ga) {
        new (From ~> From) {
          def apply[B](fb: From[B]): From[B] = underlying.local(fb)(f)
        }
      }
  }

  final class LocalLiftToOps[From[_], E] private[LocalSyntax] (private val local: Local[From, E])
      extends AnyVal {
    def liftTo[To[_]: Applicative](implicit lk: LiftKind[From, To]): Local[To, E] =
      new LiftedLocal(local)
  }
}
