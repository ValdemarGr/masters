package tt

import runtime.ReductionMachine.LCWithTrimmers._
import par.TokenTypes._
import scala.language.postfixOps

object LCTChecker {
  sealed trait HMType
  final case class HMTypeVar(name: String) extends HMType {
    override def toString = name
  }
  final case class HMTypeArr(l: HMType, r: HMType) extends HMType {
    override def toString = s"($l -> $r)"
  }
  final case class HMConst(name: HMBuiltins, params: List[HMType]) extends HMType {
    override def toString = s"($name ${params.mkString(" ")})"
  }

  final case class HMScheme(bound: Set[String], t: HMType)

  sealed trait HMBuiltins
  case object Integer extends HMBuiltins

  type Environment = Map[String, HMScheme]
  type Substitution = Map[HMTypeVar, HMType]

  def fvType(t: HMType): Set[String] = t match {
    case HMTypeVar(name) => Set(name)
    case HMTypeArr(l, r) => fvType(l) union fvType(r)
    case HMConst(_, params) => params.toSet.flatMap(fvType)
  }

  def fvPoly(sch: HMScheme) = 
    fvType(sch.t) -- sch.bound

  final case class Context(varnameCounter: Int)
  val charRange = 'a'.toInt to 'z'.toInt toArray
  def fresh(ctx: Context): (Context, HMTypeVar) = {
    val fst = charRange(ctx.varnameCounter % charRange.length).toChar.toString
    val suf = (ctx.varnameCounter / charRange.length).toString
    val newName = fst + suf
    ctx.copy(varnameCounter = ctx.varnameCounter + 1) -> HMTypeVar(newName)
  }

  def substType(s: Substitution, t: HMType): HMType = t match {
    case v:HMTypeVar => s.getOrElse(v, v)
    case HMTypeArr(l, r) => HMTypeArr(substType(s, l), substType(s, r))
    case HMConst(name, params) => HMConst(name, params.map(x => substType(s, x)))
  }

  def substEnv(s: Substitution, env: Environment): Environment = 
    env.mapValues(x => substPoly(s, x))

  def substPoly(s: Substitution, sch: HMScheme) = 
    HMScheme(sch.bound, substType(s -- sch.bound.map(x => HMTypeVar(x)), sch.t))

  def gen(env: Environment, t: HMType) = {
    val freeInEnv = env.values.toList.flatMap(fvPoly).toSet
    HMScheme(fvType(t) -- freeInEnv, t)
  }

  def inst(ctx: Context, sch: HMScheme): (Context, HMType) = {
    val (newCtx, s) = sch.bound.foldLeft[(Context, Substitution)]((ctx, Map.empty)){ case ((ctx, s), next) =>
      val (newCtx, f) = fresh(ctx)
      (newCtx, s + (HMTypeVar(next) -> f))
    }
    (newCtx, substType(s, sch.t))
  }

  def combine(s1: Substitution, s2: Substitution) = {
    s2.mapValues(t => substType(s1, t)) ++ s1
  }

  def makeMapping(tv: HMTypeVar, t: HMType): Substitution = {
    if (tv == t) {
      Map.empty
    } else if (fvType(t) contains tv.name) {
      throw new Exception(s"recursive type found $t and $tv")
    } else {
      Map(tv -> t)
    }
  }

  def unify(t1: HMType, t2: HMType): Substitution = {
    (t1, t2) match {
      case (HMTypeArr(l1, r1), HMTypeArr(l2, r2)) =>
        val s1 = unify(l1, l2)
        val s2 = unify(substType(s1, r1), substType(s1, r2))
        combine(s2, s1)
      case (tv:HMTypeVar, t) => makeMapping(tv, t)
      case (t, tv:HMTypeVar) => makeMapping(tv, t)
      case (HMConst(n1, ps1), HMConst(n2, ps2)) if n1 == n2 => 
        (ps1 zip ps2).flatMap{ case (p1, p2) => unify(p1, p2) }.toMap
      case (l, r) => throw new Exception(s"failed to unify $l and $r")
    }
  }

  def getOp(op: BuiltinOperator): HMType = op match {
    case Addition  => HMTypeArr(HMConst(Integer, Nil), HMTypeArr(HMConst(Integer, Nil), HMConst(Integer, Nil)))
    case Subtraction  => HMTypeArr(HMConst(Integer, Nil), HMTypeArr(HMConst(Integer, Nil), HMConst(Integer, Nil)))
    case Equallity  => HMTypeArr(HMConst(Integer, Nil), HMTypeArr(HMConst(Integer, Nil), HMConst(Integer, Nil)))
    case x         => throw new Exception(s"operator $x not implemented")
  }

  def entrypoint(exp: LCTExp) = infer(exp, Map.empty, Context(1))

  def infer(exp: LCTExp, env: Environment, ctx: Context): (HMType, Substitution, Context) = {
    exp match {
      case LCTVar(name) => 
        val genType: HMScheme = env.get(name).getOrElse(throw new Exception(s"did not find variable $name in env $env"))
        val (c1, t1) = inst(ctx, genType)
        (t1, Map.empty, c1)
      case LCTApplication(l, r) =>
        val (newCtx, f) = fresh(ctx)
        val (t1, s1, c1) = infer(l, env, newCtx)
        val (t2, s2, c2) = infer(r, substEnv(s1, env), c1)
        val s3 = unify(substType(s2, t1), HMTypeArr(t2, f))
        (substType(s3, f), combine(s3, combine(s2, s1)), c2)
      case LCTAbstration(param, body) => 
        val (newCtx, f) = fresh(ctx)
        //don't do this at home kids
        val introed: Environment = if (param.endsWith("''")) {
          env + (param -> HMScheme(Set(f.name), f))
        } else {
          env + (param -> HMScheme(Set.empty, f))
        }
        val (t1, s1, c1) = infer(body, introed, newCtx)
        (substType(s1, HMTypeArr(f, t1)), s1, c1)
      case LCTLet(name, body, in) => 
        //introduce monomorphic recursive name
        val (c0, f) = fresh(ctx)
        val env2 = env + (name -> HMScheme(Set.empty, f))
        val (t1, s1, c1) = infer(body, env2, c0)
        val subbed: Environment = substEnv(s1, env2)
        val genned: HMScheme = gen(subbed, t1)
        val (t2, s2, c2) = infer(in, env2 + (name -> genned), c1)
        (t2, combine(s1, s2), c2)
      case LCTIf(exp, truth, falsity) => 
        val (t1, s1, c1) = infer(exp, env, ctx)
        val (t2, s2, c2) = infer(truth, env, c1)
        val (t3, s3, c3) = infer(falsity, env, c2)
        val s4 = unify(t1, HMConst(Integer, Nil))
        val s5 = unify(t2, t3)
        (substType(s5, t2), combine(s5, combine(s4, combine(s3, combine(s2, s1)))), c3)
      case LCTNumber(n) => 
        (HMConst(Integer, Nil), Map.empty, ctx)
      case LCTBinOp(l, op, r) => 
        val (t1, s1, c1) = infer(l, env, ctx)
        val (t2, s2, c2) = infer(r, env, c1)
        val (c3, f) = fresh(c2)
        val s3 = unify(HMTypeArr(t1, HMTypeArr(t2, f)), getOp(op))
        (substType(s3, f), combine(s1, combine(s2, s3)), c3)
    }
  }
}
