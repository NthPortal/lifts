package lgbt.princess

package object lifts {
  type LiftScopeAlt[From[_], To[_]] = MapK[From, From, To, To]
}
