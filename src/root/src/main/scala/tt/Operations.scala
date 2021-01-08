package tt

import tt.Types._
import par.TokenTypes._
import scala.language.postfixOps
import cats.MonadError
import scala.collection.immutable.Nil
import cats.data.NonEmptyList

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
    //println(s"instantiate sub map is $sub for scheme $s")
    val o = Sub.substitute(s.t, sub)
    (c2, o)
  }

  def unify(t1: Type, t2: Type): Substitution =
    (t1, t2) match {
      case (TypeArrow(l1, r1), TypeArrow(l2, r2)) =>
        val s1 = unify(l1, l2)
        val s2 = unify(Sub.substitute(r1, s1), Sub.substitute(r2, s1))
        dot(s2, s1)
      case (tv @ TypeVar(_), t)                     => checkRec(tv, t)
      case (t, tv @ TypeVar(_))                     => checkRec(tv, t)
      case (TypeAtom(a1), TypeAtom(a2)) if a1 == a2 => Map.empty
      case (t1, t2)                                 => throw new Exception(s"failed to unify $t1 and $t2")
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
      case Some(Scheme(_, tn)) if (id.head.isUpper) => (ctx, tn) 
      case Some(value) => 
        instantiate(ctx, value)
      case None        => throw new Exception(s"unbound variable $id in $env")
    }

  def extend(env: Environment, id: Identifier, s: Scheme): Environment =
    env + (id -> s)

  def getOp(op: BuiltinOperator) = op match {
    case Addition  => TypeArrow(TypeAtom(AInt), TypeArrow(TypeAtom(AInt), TypeAtom(AInt)))
    case Equallity => TypeArrow(TypeAtom(AInt), TypeArrow(TypeAtom(AInt), TypeAtom(ABool)))
    case _         => ???
  }

  def inferAppParams(ctx: Context,
                     sub: Substitution,
                     env: Environment,
                     ps: List[Expression]): (Context, Substitution, Type) =
    ps match {
      case Nil =>
        val (c, f) = fresh(ctx)
        (c, sub, f)
      case head :: tl =>
        val newEnv = env.mapValues(x => Sub.substitute(x, sub))
        val (c1, s1, t1) = inferExpr(ctx, sub, newEnv, head)
        val (c2, s2, t2) = inferAppParams(c1, s1, newEnv, tl)
        val newT = TypeArrow(t1, t2)
        (c2, s2, newT)
    }

  def unrollArr(t: Type): NonEmptyList[Type] = t match {
    case TypeArrow(lh, rh) => NonEmptyList.of(lh) concatNel unrollArr(rh)
    case x => NonEmptyList.of(x)
  }

  def zipPair(ctx: Context, sub: Substitution, env: Environment, t: Type, x: Identifier) = {
    val (c1, tv) = fresh(ctx)
    val sc = generalize(env, tv)
    val newEnv = extend(env, x, sc)
    val s1 = unify(tv, t)
    (c1, s1, newEnv)
  }

  def unifyZip(ctx: Context,
               sub: Substitution,
               env: Environment,
               xs: List[(Type, Identifier)]): (Context, Substitution, Environment) =
                 xs match {
      case ((t, id) :: tail) =>
        val (c1, s1, e1) = zipPair(ctx, sub, env, t, id)
        unifyZip(c1, s1, e1, tail)
      case Nil =>
        (ctx, sub, env)
    }

  def getTypeConstructorType(env: Environment, c: MatchCase) =
    env
      .get(c.typeConstructor)
      .getOrElse(throw new Exception(s"failed to find typeConstructor for match case ${c} in env $env"))

  def inferPatternMatch(ctx: Context, sub: Substitution, env: Environment, cases: List[MatchCase], tcName: Identifier): (Context, Substitution, Type) =
    cases match {
      case Nil => 
        val (c1, f) = fresh(ctx)
        (c1, sub, f)
      case head :: tl =>
        val tc = getTypeConstructorType(env, head).t
        val unrolled = unrollArr(tc)
        val uninionType = unrolled.last match {
          case TypeAtom(tv@AADT(_)) => tv
          case x => throw new Exception(s"failed to match type $x to typevar")
        }
        if (uninionType.name != tcName) {
          throw new Exception(s"found unexpected typeConstructor, expected $tcName, found ${uninionType.name}")
        } else {
          val comb = unrolled.toList.dropRight(1).zip(head.bindings)
          val (c1, s1, e1) = unifyZip(ctx, sub, env, comb)
          val (c2, s2, t) = inferType(c1, s1, e1, head.body)
          val (c3, s3, t3) = inferPatternMatch(c2, dot(s2, s1), env, tl, tcName)
          val s4 = unify(t, t3)
          //println(s"t $t t3 $t3 s4 $s4 env $env comb $comb")
          (c3, dot(s1, dot(s2, dot(s3, s4))), t)
        }
    }

  def inferExpr(ctx: Context, sub: Substitution, env: Environment, e: Expression): (Context, Substitution, Type) =
    e match {
      case _: ConstantInteger => (ctx, sub, TypeAtom(AInt))
      case Apply(name, ps) =>
        val (c1, ot) = findVar(ctx, env, name)

        ps match {
          case Nil => (c1, sub, ot)
          case head :: tl =>
            val (c2, s2, t2) = inferAppParams(c1, sub, env, head :: tl)
            val subOt = Sub.substitute(ot, s2)
            val un = unify(subOt, t2)
            val l = unrollArr(t2).last
            //println(s"applying $ps to $name creates substitution set ${dot(un, dot(s2, sub))} and type $l by pre unification $subOt and $t2 and env $env")
            (c2, dot(un, dot(s2, sub)), Sub.substitute[Type](l, un))
        }
      case If(expr, fst, snd) =>
        val (c1, s1, t1) = inferExpr(ctx, sub, env, expr)
        val (c2, s2, t2) = inferType(c1, sub, env, fst)
        val (c3, s3, t3) = inferType(c2, sub, env, snd)

        val s4 = unify(t1, TypeAtom(ABool))
        val s5 = unify(t2, t3)
        (c3, dot(s5, dot(s4, dot(s3, dot(s2, s1)))), Sub.substitute(t2, s5))
      case InfixBuiltin(lhs, op, rhs) =>
        val (c1, s1, t1) = inferExpr(ctx, sub, env, lhs)
        val (c2, s2, t2) = inferExpr(c1, sub, env, rhs)
        val (c3, f) = fresh(c2)
        val binF = TypeArrow(t1, TypeArrow(t2, f))
        val s3 = unify(binF, getOp(op))
        (c3, dot(s1, dot(s2, s3)), Sub.substitute[Type](f, s3))
      case PatternMatch(e, cases) =>
        val (c1, s1, t1) = inferExpr(ctx, sub, env, e)
        val fc = getTypeConstructorType(env, cases.head).t
        val unrolled = unrollArr(fc)
        val uninionType = unrolled.last match {
          case TypeAtom(tv@AADT(_)) => tv
          case x => throw new Exception(s"failed to match type $x to typevar")
        }
        val s2 = unify(TypeAtom(uninionType), t1)
        inferPatternMatch(c1, dot(s1, s2), env, cases.toList, uninionType.name)
    }

  def dot(s1: Substitution, s2: Substitution): Substitution =
    s2.mapValues(x => Sub.substitute(x, s1)) ++ s1

  def inferFun(ctx: Context,
               sub: Substitution,
               env: Environment,
               ids: List[Identifier],
               b: FunctionBody): (Context, Substitution, Type) =
    ids match {
      case Nil => inferType(ctx, sub, env, b)
      case head :: tl =>
        val (c1, f) = fresh(ctx)
        val newEnv = extend(env, head, Scheme(Set.empty, f))
        val (c2, s2, innerT) = inferFun(c1, sub, newEnv, tl, b)
        (c2, s2, Sub.substitute[Type](TypeArrow(f, innerT), s2))
    }

    def countTypes(t: Type): Int = t match {
      case TypeArrow(lh, rh) => countTypes(lh) + countTypes(rh)
      case TypeVar(typename) => 1
      case TypeAtom(a) => 1
    }

  def inferTypeConstructor(tps: Set[Identifier], name: Identifier, tt: List[TypeParam]): Type = tt match {
    case Nil => TypeAtom(AADT(name))
    case head :: tl =>
      head match {
        case TypeName(cin) =>
          if (tps.contains(cin)) {
            TypeArrow(TypeVar(cin), inferTypeConstructor(tps, name, tl))
          } else {
            throw new Exception(s"failed to find any type for $cin")
          }
      }
  }

  def inferDecl(ctx: Context,
                sub: Substitution,
                env: Environment,
                d: Declaration): (Context, Substitution, Environment) = d match {
    case TypeDeclaration(typename, typeParams, expr) =>
      val newEnv = expr.types.foldLeft(env) {
        case (accum, next) =>
          val paramType = inferTypeConstructor(typeParams.toSet, typename, next.ids)
          val scheme = generalize(accum, paramType)
          extend(accum, next.name, scheme)
      }
      (ctx, sub, newEnv)
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
      println(s"########## $varname ##########")
      println(s"substitution set $s1\ntype $t1\ncurrent env is $eo")
      println(s"Type vars = ${c1.varnameCounter}")
      println(s"Type vars in sub = ${s1.map{ case (_, t) => countTypes(t) + 1}.sum}")
      println(s"########## $varname ##########")
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

  def inferProgram(dls: List[Declaration]) = {
    val pesudoExpr = Apply("main", Nil)
    val fb = FunctionBody(dls, pesudoExpr)
    val (_, s1, programType) = inferType(Context(0), Map.empty, Map.empty, fb)
    val s2 = unify(TypeAtom(AInt), programType)
    Sub.substitute(programType, s2)
  }
}
