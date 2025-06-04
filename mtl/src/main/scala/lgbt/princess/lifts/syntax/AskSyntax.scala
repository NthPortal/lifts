package lgbt.princess.lifts
package syntax

import cats.Applicative
import cats.mtl.Ask

trait AskSyntax {
  implicit def toAskLiftToOps[From[_], E](ask: Ask[From, E]): AskSyntax.AskLiftToOps[From, E] =
    new AskSyntax.AskLiftToOps(ask)
}

object AskSyntax extends AskSyntax {
  private[this] final class LiftedAsk[From[_], To[_], E](
      underlying: Ask[From, E]
  )(implicit G: Applicative[To], lv: LiftValue[From, To])
      extends Ask[To, E] {
    val applicative: Applicative[To] = G
    def ask[E2 >: E]: To[E2] = lv.liftF(underlying.ask[E2])
  }

  final class AskLiftToOps[From[_], E] private[AskSyntax] (private val ask: Ask[From, E])
      extends AnyVal {
    def liftTo[To[_]: Applicative](implicit lv: LiftValue[From, To]): Ask[To, E] =
      new LiftedAsk(ask)
  }
}
