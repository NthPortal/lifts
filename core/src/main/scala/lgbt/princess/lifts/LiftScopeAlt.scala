package lgbt.princess.lifts

object LiftScopeAlt {
  def apply[F[_], G[_]](implicit ls: LiftScopeAlt[F, G]): LiftScopeAlt[F, G] = ls
}
