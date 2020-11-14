package tt

import par.TokenTypes._
import cats.Show
import cats.kernel.Monoid
import cats.data.State
import cats.Monad

object Types {
  type Assumption = Map[Identifier, Scheme]
  type MonoVars = Set[TypeVar]
  type Substitution = Map[TypeVar, Type]
  
  case class Scheme(typevars: Set[TypeVar], t: Type)

  sealed trait Ctx
  case class Context(varnameCounter: Int) extends Ctx
  case class Fail(msg: String) extends Ctx
  

  sealed trait Atom
  case object AInt extends Atom
  implicit val atomShow = Show.show[Atom]{ 
    case AInt => "Int"
  }

  sealed trait Type
  case class TypeArrow(lh: Type, rh: Type) extends Type
  case class TypeVar(typename: String) extends Type
  case class TypeAtom(a: Atom) extends Type
}
