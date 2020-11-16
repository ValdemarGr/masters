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
      Free.freevars(s.t).diff(s.typevars)
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
  implicit object SSub extends Sub[Scheme] {
    def substitute(a: Scheme, s: Substitution): Scheme =
      Scheme(a.typevars, Sub.substitute(a.t, s -- a.typevars))
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

  def unify(t1: Type, t2: Type): Substitution =
    (t1, t2) match {
      case (TypeArrow(l1, r1), TypeArrow(l2, r2)) => 
        val s1 = unify(l1, l2)
        val s2 = unify(Sub.substitute(r1, s1), Sub.substitute(r2, s1))
        dot(s2, s1)
      case (tv@TypeVar(_), t) => checkRec(tv, t)
      case (t, tv@TypeVar(_)) => checkRec(tv, t)
      case (TypeAtom(a1), TypeAtom(a2)) if a1 == a2 => Map.empty
      case (t1, t2) => throw new Exception(s"failed to unify $t1 and $t2")
    }

  def checkRec(tv: TypeVar, t: Type): Substitution =
    if (tv == t) {
      Map.empty
    } else if (Free.freevars(t).contains(tv))
      throw new Exception(s"recursive definition found")
    else {
      Map(tv -> t)
    }

  def findVar(ctx: Context, env: Environment, id: Identifier): (Context, Type) =
    env.get(id) match {
      case Some(value) => instantiate(ctx, value)
      case None        => throw new Exception(s"unbound variable $id in $env")
    }

  def extend(env: Environment, id: Identifier, s: Scheme): Environment = 
    env + (id -> s)

  def getOp(op: BuiltinOperator) = op match {
    case Addition => TypeArrow(TypeAtom(AInt), TypeArrow(TypeAtom(AInt), TypeAtom(AInt)))
    case _ => ???
  }

  def inferExpr(ctx: Context, sub: Substitution, env: Environment, e: Expression): (Context, Substitution, Type) =
    e match {
      case _: ConstantInteger => (ctx, sub, TypeAtom(AInt))
      case Apply(name, ps) =>
        val (c1, f) = fresh(ctx)
        val (c2, t1) = findVar(c1, env, name)
        (c2, sub, t1)
      case InfixBuiltin(lhs, op, rhs) => 
        val (c1, s1, t1) = inferExpr(ctx, sub, env, lhs)
        val (c2, s2, t2) = inferExpr(c1, sub, env, rhs)
        val (c3, f) = fresh(c2)
        val binF = TypeArrow(t1, TypeArrow(t2, f))
        val s3 = unify(binF, getOp(op))
        (c3, dot(s1, dot(s2, s3)), Sub.substitute[Type](f, s3))
    }

  def dot(s1: Substitution, s2: Substitution): Substitution = 
    s2 ++ s1 mapValues (x => Sub.substitute(x, s1))

  def inferFun(ctx: Context, sub: Substitution, env: Environment, ids: List[Identifier], b: FunctionBody): (Context, Substitution, Type) = 
    ids match {
      case Nil => inferType(ctx, sub, env, b)
      case head :: tl =>
        val (c1, f) = fresh(ctx)
        val newEnv = extend(env, head, Scheme(Set.empty, f))
        val (c2, s2, innerT) = inferFun(c1, sub, newEnv, tl, b)
        (c2, s2, Sub.substitute[Type](TypeArrow(f, innerT), s2))
    }
  

  def inferDecl(ctx: Context,
                sub: Substitution,
                env: Environment,
                d: Declaration): (Context, Substitution, Environment) = d match {
    case LetDecl(varname, value) =>
      val (c1, s1, t1) = inferExpr(ctx, sub, env, value)
      val newEnv = env.mapValues(x => Sub.substitute(x, s1))
      val newt = generalize(newEnv, t1)
      val extededEnv = extend(env, varname, newt)
      (c1, s1, extededEnv)
    case FunDecl(varname, params, body) => 
      val (c1, s1, t1) = inferFun(ctx, sub, env, params.map(_.id), body)
      val gen = generalize(env, t1)
      val eo = extend(env, varname, gen)
      (c1, s1, eo)
    case _ => ???
  }

  def inferType(ctx: Context, sub: Substitution, env: Environment, b: FunctionBody): (Context, Substitution, Type) =
    b.children match {
      case Nil => inferExpr(ctx, sub, env, b.end)
      case head :: tl =>
        val bodyInf = tl.foldLeft(inferDecl(ctx, sub, env, head)) {
          case ((ctx, sub, env), next) =>
            inferDecl(ctx, sub, env, next)
        }
        val (bc, bs, be) = bodyInf
        val (c2, s2, t2) = inferExpr(bc, bs, be, b.end)
        (c2, dot(bs, s2), t2)
    }
}
