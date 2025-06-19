package lgbt.princess.lifts

object LiftScopeAlt {
  type Derived[From[_], Wrapper[_[_], _]] = LiftScopeAlt[From, Wrapper[From, *]]

  def apply[From[_], To[_]](implicit ev: LiftScopeAlt[From, To]): LiftScopeAlt[From, To] = ev
}
