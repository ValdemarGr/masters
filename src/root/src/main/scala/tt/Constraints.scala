package tt

import tt.Types._

object Constraints {
  type ConstraintSet = Set[Constraint]

  sealed trait Constraint
  case class Equivalent(t1: Type, t2: Type) extends Constraint
  case class ExInstance(lhs: TypeVar, rhs: TypeVar) extends Constraint
  case class ImInstance(lhs: TypeVar, monoVars: MonoVars, rhs: TypeVar) extends Constraint
}
