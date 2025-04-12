package lgbt.princess

package object lifts {
  type LiftScopeAlt[F[_], G[_]] = MapK[F, F, G, G]
}
