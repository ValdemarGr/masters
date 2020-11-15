package tt

import tt.Types._
import par.TokenTypes._
import scala.language.postfixOps
import cats.MonadError
import scala.collection.immutable.Nil

object Operations {
  // typeclasses for freevar
  sealed trait Free[A] {
    def freevars(a: A): Set[TypeVar]
  }
  object Free {
    def freevars[A](a: A)(implicit F: Free[A]) = F.freevars(a)
  }

  implicit object TFree extends Free[Type] {
    def freevars(a: Type): Set[TypeVar] =
      a match {
        case TypeArrow(lh, rh) => Free.freevars(lh) | Free.freevars(rh)
        case x: TypeVar        => Set(x)
        case TypeAtom(a)       => Set.empty
      }
  }
  implicit object SFree extends Free[Scheme] {
    def freevars(s: Scheme): Set[TypeVar] =
      Free.freevars(s).diff(s.typevars)
  }

  // typeclasses for substitution
  sealed trait Sub[A] {
    def substitute(a: A, s: Substitution): A
  }
  object Sub {
    def substitute[A](a: A, s: Substitution)(implicit S: Sub[A]): A = S.substitute(a, s)
  }
  implicit object TSub extends Sub[Type] {
    def substitute(a: Type, s: Substitution): Type = a match {
      case TypeArrow(lh, rh) => TypeArrow(Sub.substitute(lh, s), Sub.substitute(rh, s))
      case t: TypeVar        => s.getOrElse(t, t)
      case a: TypeAtom       => a
    }
  }

  val charRange = 'a'.toInt to 'z'.toInt toArray
  def fresh(ctx: Context): (Context, TypeVar) = {
    val fst = charRange(ctx.varnameCounter % charRange.length).toChar.toString
    val suf = (ctx.varnameCounter / charRange.length).toString
    val newName = fst + suf
    ctx.copy(varnameCounter = ctx.varnameCounter + 1) -> TypeVar(newName)
  }

  def generalize(env: Environment, t: Type): Scheme = {
    val eS = env.values.toList.flatMap(Free.freevars[Scheme]).toSet
    Scheme(Free.freevars(t) -- eS, t)
  }

  def instantiate(ctx: Context, s: Scheme): (Context, Type) = {
    val asl = s.typevars.toList
    val (c2, sub) = asl.foldLeft(ctx -> Map.empty[TypeVar, Type]) {
      case ((c, m), n) =>
        val (nc, nt) = fresh(c)
        nc -> (m + (n -> nt))
    }
    val o = Sub.substitute(s.t, sub)
    (c2, o)
  }

  def inferType(ctx: Context, env: Environment, b: List[Declaration]) = {
    def inferExpr(): (Context, Substitution, Type) = ???

    b match {
      case Nil => 
      case head :: tl =>
    }
  }
}
