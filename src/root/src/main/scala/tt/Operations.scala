package tt

import tt.Types._
import par.TokenTypes._
import scala.language.postfixOps
import cats.MonadError
import scala.collection.immutable.Nil
import tt.Constraints._

object Operations {
  // typeclasses for freevar
  sealed trait Free[A] {
    def freevars(a: A): MonoVars
  }
  object Free {
    def freevars[A](a: A)(implicit F: Free[A]) = F.freevars(a)
  }

  implicit object TFree extends Free[Type] {
    def freevars(a: Type): MonoVars =
      a match {
        case TypeArrow(lh, rh) => Free.freevars(lh) | Free.freevars(rh)
        case x: TypeVar        => Set(x)
        case TypeAtom(a)       => Set.empty
      }
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

  def generalize(m: MonoVars, t: Type): Scheme =
    Scheme(Free.freevars(t) -- m, t)

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

  def getvar[F[_]](ctx: Context, a: Assumption, id: Identifier)(
    implicit M: MonadError[F, Throwable]
  ): F[(Context, Type)] =
    a.get(id) match {
      case Some(value) => M.pure(instantiate(ctx, value))
      case None        => M.raiseError[(Context, Type)](new Exception(s"failed to find $id in type envrionment ${a}"))
    }

  def typer[F[_]](ctx: Context, a: Assumption, c: ConstraintSet, funB: FunctionBody)(
    implicit M: MonadError[F, Throwable]
  ) = {
    def typeExpr(e: Expression): Constraint = e match {
      case Apply(name, vs) => 
        vs match {
          case Nil => 
          case head :: tl => 
        }
      case ConstantInteger(v) => TypeAtom(AInt)
      case InfixBuiltin(lhs, op, rhs) => 
        val l = typeExpr(lhs)
        val r = typeExpr(rhs)
        val (_, f) = fresh(ctx)
        val asFT = TypeArrow(l, TypeArrow(l, f))
        val opType = op match {
          case Addition => 
            val it = TypeAtom(AInt)
            TypeArrow(it, TypeArrow(it, it))
        }
        Equivalent(opType, asFT)
    }

    def typeDecl(d: Declaration): (Identifier, Type) = d match {
      case LetDecl(name, expr) => name -> typeExpr(expr)
      case _                   => ???
    }

    funB.children match {
      case Nil => typeExpr(funB.end)
      case head :: tl =>
        val lets = tl.foldLeft(typeDecl(head)) {
          case (accum, next) =>
            typeDecl(next)
        }
        typeExpr(funB.end)
    }
  }
}
