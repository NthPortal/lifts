package lgbt.princess.lifts
package syntax

import cats.Applicative
import cats.mtl.Ask

trait AskSyntax {
  implicit def toAskLiftToOps[F[_], E](ask: Ask[F, E]): AskSyntax.AskLiftToOps[F, E] =
    new AskSyntax.AskLiftToOps(ask)
}

object AskSyntax extends AskSyntax {
  private[this] final class LiftedAsk[F[_], G[_], E](
      underlying: Ask[F, E]
  )(implicit G: Applicative[G], lv: LiftValue[F, G])
      extends Ask[G, E] {
    val applicative: Applicative[G] = G
    def ask[E2 >: E]: G[E2] = lv.liftF(underlying.ask[E2])
  }

  final class AskLiftToOps[F[_], E] private[AskSyntax] (private val ask: Ask[F, E]) extends AnyVal {
    def liftTo[G[_]: Applicative](implicit lv: LiftValue[F, G]): Ask[G, E] =
      new LiftedAsk(ask)
  }
}
