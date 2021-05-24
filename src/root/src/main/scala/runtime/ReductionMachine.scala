package runtime

import ir.LCLanguage._
import par.TokenTypes._
import scala.collection.immutable.Stack
import scala.language.postfixOps
import scala.annotation.tailrec
import cats.Eval
import scala.collection.immutable.Nil
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Await

object ReductionMachine {
  object LCWithTrimmers {
    sealed trait LCTExp { def trimmer: Set[String] }

    final case class LCTVar(name: String) extends LCTExp {
      val trimmer = Set(name)
      override def toString = s"$name"
    }

    final case class LCTApplication(l: LCTExp, r: LCTExp) extends LCTExp {
      val trimmer = l.trimmer union r.trimmer
      override def toString = s"($l $r)"
    }

    final case class LCTAbstration(param: String, body: LCTExp) extends LCTExp {
      val trimmer = body.trimmer - param
      override def toString = s"\\$param.$body"
    }

    final case class LCTLet(name: String, body: LCTExp, in: LCTExp) extends LCTExp {
      val trimmer = (body.trimmer union in.trimmer) - name
      override def toString = s"let $name = ($body) in $in"
    }

    final case class LCTIf(exp: LCTExp, truth: LCTExp, falsity: LCTExp) extends LCTExp {
      val trimmer = (exp.trimmer union truth.trimmer union falsity.trimmer)
      override def toString = s"if ($exp) ($truth) else ($falsity)"
    }

    final case class LCTNumber(n: Int) extends LCTExp {
      val trimmer = Set.empty
      override def toString = s"$n"
    }

    final case class LCTBinOp(l: LCTExp, op: BuiltinOperator, r: LCTExp) extends LCTExp {
      val trimmer = l.trimmer union r.trimmer
      override def toString = s"($l $op $r)"
    }

    def fromLCTerms(lc: LCExp): LCTExp = lc match {
      case LCName(name) => LCTVar(name)
      case LCApplication(l, p) => LCTApplication(fromLCTerms(l), fromLCTerms(p))
      case LCFunction(_, name, body) => LCTAbstration(name.name, fromLCTerms(body))
      case LCNumber(n) => LCTNumber(n)
      case LCIf(exp, fst, snd) => LCTIf(fromLCTerms(exp), fromLCTerms(fst), fromLCTerms(snd))
      case LCLet(name, exp, in) => LCTLet(name, fromLCTerms(exp), fromLCTerms(in))
      case LCTerminalOperation(l, op, r) => LCTBinOp(fromLCTerms(l), op, fromLCTerms(r))
    }
  }
  import LCWithTrimmers._

  type Variable = String
  type Heap = Map[Variable, LCTExp]
  type Timestamp = Int
  type Environment = Map[String, String]

  class DualHeap(
    val live: Map[Variable, (LCTExp, Environment, Timestamp)],
    val idle: Map[Variable, (LCTExp, Environment, Timestamp)]
  ) {
    def size = live.size + idle.size
    def get(name: Variable): (LCTExp, Environment, Timestamp) = 
      (live.get(name), idle.get(name)) match {
        case (Some((e1, env1, t1)), Some((e2, env2, t2))) =>
          if (t1 > t2) (e1, env1, t1)
          else (e2, env2, t2)
        case (o1, o2) =>
          o1.orElse(o2).getOrElse(throw new Exception(s"failed to find $name in dual heap"))
      }
    def insert(name: Variable, value: LCTExp, env: Environment, ctx: Context): (DualHeap, Context) = {
      val nv = (name, (value, env, ctx.nextLamport))
      val nh = new DualHeap(live + nv, idle)
      (nh, ctx.copy(nextLamport = ctx.nextLamport + 1))
    }
    def swap = new DualHeap(idle, live)
  }

  final case class ContinuationParams(
    gamma: Heap,
    stack: ContStack,
    exp: LCTExp,
    ctx: Context
  )
  type Continuation = ContinuationParams => LCTExp
  type ContStack = List[Continuation]
  final case class Context(freshNum: Int, nextLamport: Int)

  val charRange = 'a'.toInt to 'z'.toInt toArray
  def fresh(ctx: Context): (Context, String) = {
    val fst = charRange(ctx.freshNum % charRange.length).toChar.toString
    val suf = (ctx.freshNum / charRange.length).toString
    val newName = fst + suf
    (ctx.copy(freshNum = ctx.freshNum + 1), newName)
  }

  //def subst(f: Variable, t: Variable, exp: LCTExp): LCTExp = exp match {
    //case LCFunction(m, x, e) =>
      //if (x.name == f) exp
      //else LCFunction(m, x, subst(f, t, e))
    //case LCName(x) =>
      //if (x == f) LCName(t)
      //else LCName(x)
    //case LCApplication(fst, snd) =>
      //LCApplication(subst(f, t, fst), subst(f, t, snd))
    //case LCTerminalOperation(lh, op, rh) =>
      //LCTerminalOperation(subst(f, t, lh), op, subst(f, t, rh))
    //case LCIf(exp, fst, snd) =>
      //LCIf(subst(f, t, exp), subst(f, t, fst), subst(f, t, snd))
    //case LCLet(name, exp, in) =>
      //if (name == f) LCLet(name, exp, in)
      //else LCLet(name, subst(f, t, exp), subst(f, t, in))
    //case x => x
  //}

  //def continue(gamma: Heap, stack: ContStack, exp: LCTExp, ctx: Context): LCTExp = stack match {
  //case Nil => exp
  //case x :: xs => x(ContinuationParams(gamma, xs, exp, ctx))
  //}

  def run(program: LCTExp): LCTExp = {
    val gamma: DualHeap = new DualHeap(Map.empty, Map.empty)
    val stack: ContStack = List.empty
    val ctx = Context(1, 1)
    timeAtStart = System.currentTimeMillis
    val (heapout, out, _, _, _, gc) = eval(gamma, program, Nil, Map.empty, ctx, None).value
    println(s"final heap was size ${heapout.size}")
    //import scala.concurrent.duration._
    //println("beginning final gc")
    //val swapped1 = heapout.swap
    //val gc1r = parallelGarbageCollect(swapped1, Nil, Map.empty)
    //val swapped2 = new DualHeap(gc1r, swapped1.idle)
    //val gc2r = parallelGarbageCollect(swapped2, Nil, Map.empty)
    //val outH = new DualHeap(gc2r, gc1r)
    //println(s"s2 had ${swapped2.size}, outH had ${outH.size}")
    //val futcomp = Await.result(gc.get, 10.second)
    //println(s"future completed with $futcomp")
    out
  }

  type N = List[(LCTExp, Environment)]

  def freeVars(e: LCTExp): Set[String] = e match {
    case LCTVar(name) => Set(name)
    case LCTApplication(l, p) =>
      freeVars(l).union(freeVars(p))
    case LCTIf(e, f, s) => freeVars(e).union(freeVars(f)).union(freeVars(s))
    case LCTAbstration(x, e) =>
      freeVars(e) - x
    case LCTLet(x, e, p) =>
      (freeVars(e).union(freeVars(p))) - x
    case LCTNumber(_)                  => Set.empty
    case LCTBinOp(l, _, r) => freeVars(l).union(freeVars(r))
  }

  //def syncGarbageCollect(gamma: Heap, n: N, accum: Heap): Heap = n match {
    //case Nil => accum
    //case x :: xs =>
      //val frees = x.trimmer
      //val (toKeep, rest) = gamma.partition { case (k, _) => frees.contains(k) }
      //syncGarbageCollect(rest, toKeep.values.toList ++ xs, toKeep ++ accum)
  //}

  def parallelGarbageCollect(dh: DualHeap, n: N, accum: Map[Variable, (LCTExp, Environment, Timestamp)]): Map[Variable, (LCTExp, Environment, Timestamp)] = n match {
    case Nil => 
      accum
    case (x, env) :: xs =>
      //println(s"got ns ${n.size}")
      val frees  = x.trimmer.map(env.get(_).get)
      val toKeepIdle = dh.idle.filter{ case (k, _) => frees.contains(k) }
      val toKeepLive = dh.live.filter{ case (k, _) => frees.contains(k) }
      def newest(one: Map[Variable, (LCTExp, Environment, Timestamp)], two: Map[Variable, (LCTExp, Environment, Timestamp)]) = {
        type T = Either[(Variable, (LCTExp, Environment, Timestamp)), (Variable, (LCTExp, Environment, Timestamp))]
        val setEither: Set[T] = (one.keySet ++ two.keySet).map{ k =>
          (one.get(k), two.get(k)) match {
            case (Some((e1, env1, t1)), Some((e2, env2, t2))) =>
              if (t1 > t2) Left((k, (e1, env1, t1)))
              else Right((k, (e2, env2, t2)))
            case (o1, o2) => 
              val o1format: Option[T] = o1
                .map(v1 => Left((k, v1)))
              val o2format: Option[T] = o2
                .map(v2 => Right((k, v2)))
              val combined: Option[T] = o1format.orElse(o2format)
              combined.getOrElse(throw new Exception(s"failed to find any key for $k"))
          }
        }
        val rights = setEither.collect{ case Right(x) => x }.toMap
        val lefts = setEither.collect{ case Left(x) => x }.toMap
        (lefts, rights)
      }
      val (newIdle, newLive) = newest(toKeepIdle, toKeepLive)
      val n2 = newIdle ++ newLive
      //println(s"n2 size ${n2.size} vs in ${(toKeepLive ++ toKeepIdle).size}")
      val r = n2.values.toList
      parallelGarbageCollect(new DualHeap(
        dh.live.filter{ case (x, _) => !n2.contains(x)},
        dh.idle.filter{ case (x, _) => !n2.contains(x)},
      ), r.map{ case (exp, env, _) => (exp, env) } ++ xs, newIdle ++ accum)
  }

  type GCFuture = Option[Future[Map[Variable, (LCTExp, Environment, Timestamp)]]]
  val sync = "sync"
  val par = "parallel"
  val none = "none"
  val gcStrategy = sync
  var memUsage = false
  //var timeAtLast: Long = 0
  var timeAtStart: Long = 0
  def eval(he: DualHeap, exp: LCTExp, ns: N, env: Environment, ctx: Context, gf: GCFuture): Eval[(DualHeap, LCTExp, N, Environment, Context, GCFuture)] = {
    //println(s"$exp")
    //val currentTime = System.currentTimeMillis()
    //val deltams = 100 
    //if (currentTime - deltams > timeAtLast) {
      //println(s"current heap size is ${he.size}")
      //timeAtLast = System.currentTimeMillis
    //}
    //println(s"runnig with $gamma $stack $exp $ctx")
    val (gamma, gcFuture) = if (math.random() < 0.001) {
      if (gcStrategy == sync) {
        //val newHeap = syncGarbageCollect(he.live.mapValues(_._1), exp :: ns, Map.empty)
        val newHeap = parallelGarbageCollect(he.swap, (exp, env) :: ns, Map.empty)
          val t = System.currentTimeMillis - timeAtStart
          //println(s"(${t}, ${he.size}) (${t + 1}, ${newHeap.size})")
        (new DualHeap(newHeap, Map.empty), None)
        //(new DualHeap(newHeap.mapValues(e => (e, 0)), Map.empty), None)
      } else if (gcStrategy == none) {
        (he, None)
      } else {
        val (newHe, futGc) = gf match {
          case None => 
            //println(s"beginning gc at ${he.size}")
            val swapped = he.swap
            (swapped, Future(parallelGarbageCollect(swapped, (exp, env) :: ns, Map.empty)))
          case Some(value) => 
            (he, value)
        }
        if (futGc.isCompleted) {
          import scala.concurrent.duration._
          val newIdle = Await.result(futGc, 1.second)
          //println(s"gc generated ${newIdle.size}, current has ${newHe.idle.size}")
          val dhprime = new DualHeap(newHe.live, newIdle)
          //println(s"total size is ${dhprime.size}, previous was ${he.size} eval time ${System.currentTimeMillis - timeAtStart}")
          val t = System.currentTimeMillis - timeAtStart
          //println(s"(${t}, ${he.size}) (${t + 1}, ${dhprime.size})")
          (dhprime, None)
        } else {
          (newHe, Some(futGc))
        }
      }
    } else {
      //println(s"didin't gc, size is ${he.size}")
      (he, gf)
    }
    exp match {
      case LCTIf(e, fst, snd) =>
        Eval.defer(eval(gamma, e, (fst, env) :: ((snd, env) :: ns), env, ctx, gcFuture)).flatMap {
          case (sigma, LCTNumber(n), ns, env, ctx, gcFuture) =>
            val next = if (n == 1) fst else snd
            Eval.defer(eval(sigma, next, ns.drop(2), env, ctx, gcFuture))
          case _ => ???
        }
      case e: LCTAbstration => Eval.later((gamma, e, ns, env, ctx, gcFuture))
      case LCTApplication(l, p) =>
        p match {
          case LCTVar(name) =>
            val pValue = env.get(name).get
            Eval.defer(eval(gamma, l, (p, env) :: ns, env, ctx, gcFuture)).flatMap {
              case (sigma, LCTAbstration(x, e), ns, env, ctx, gcFuture) =>
                Eval.defer(eval(sigma, e, ns.drop(1), env + (x -> pValue), ctx, gcFuture))
          case _ => ???
            }
          case other =>
            val (nctx, fr) = fresh(ctx)
            val bound = LCTLet(fr, p, LCTApplication(l, LCTVar(fr)))
            Eval.defer(eval(gamma, bound, ns, env, nctx, gcFuture))
        }
      case LCTVar(name) =>
        val envMapping = env.get(name).get
        val (gotten, env2, _) = gamma.get(envMapping)
        Eval.defer(eval(gamma, gotten, ns, env2, ctx, gcFuture)).flatMap {
          case (sigma, e, ns, env2, ctx, gcFuture) =>
            val (newheap, nctx) = sigma.insert(envMapping, e, env2, ctx)
            Eval.later((newheap, e, ns, env2, nctx, gcFuture))
        }
      case LCTLet(x, e, p) =>
        val (nctx, fr) = fresh(ctx)
        val envWithFresh = env + (x -> fr)
        val (newheap, ctx2) = gamma.insert(fr, e, envWithFresh, nctx)
        Eval.defer(eval(newheap, p, ns, envWithFresh, ctx2, gcFuture))
      case LCTBinOp(x, op, y) =>
        Eval.defer(eval(gamma, x, (y, env) :: ns, env, ctx, gcFuture)).flatMap {
          case (theta, LCTNumber(n), ns, env1, ctx1, gcFuture) =>
            Eval.defer(eval(theta, y, ns.drop(1), env, ctx1, gcFuture)).flatMap {
              case (sigma, LCTNumber(t), ns, env, ctx2, gcFuture) =>
                val expres =
                  op match {
                    case Addition    => n + t
                    case Subtraction => n - t
                    case Equallity   => if (n == t) 1 else 0
                    case o           => throw new Exception(s"didnt implement operator for $o between $n and $t")
                  }
                Eval.later((sigma, LCTNumber(expres), ns, env, ctx2, gcFuture))
          case _ => ???
            }
          case _ => ???
        }
      case n: LCTNumber => Eval.later((gamma, n, ns, env, ctx, gcFuture))
    }
  }

  //scala does not support tail call optimizaiton
  //def eval(gamma: Heap, stack: ContStack, exp: LCExp, ctx: Context): LCExp = {
  ////println(s"runnig with $gamma $stack $exp $ctx")
  //exp match {
  //case LCIf(e, fst, snd) =>
  //val cont: Continuation = params => {
  //val nextExp = params.exp match {
  //case LCNumber(n) =>
  //if (n == 1) fst
  //else snd
  //}
  //eval(params.gamma, params.stack, nextExp, params.ctx)
  //}
  //eval(gamma, cont :: stack, e, ctx)
  //case e: LCFunction => continue(gamma, stack, exp, ctx)
  //case LCApplication(l, p) =>
  //p match {
  //case LCName(name) =>
  //val cont: Continuation = params =>
  //params.exp match {
  //case LCFunction(_, x, e) =>
  //eval(params.gamma, params.stack, subst(x.name, name, e), params.ctx)
  //}
  //eval(gamma, cont :: stack, l, ctx)
  //case other =>
  //val (nctx, fr) = fresh(ctx)
  //val bound = LCLet(fr, p, LCApplication(l, LCName(fr)))
  //eval(gamma, stack, bound, nctx)
  //}
  //case LCName(name) =>
  //val gotten = gamma.get(name).getOrElse(throw new Exception(s"failed to find variable $name in gamma $gamma"))
  //val cont: Continuation = params =>
  //continue(params.gamma + (name -> params.exp), params.stack, params.exp, params.ctx)
  //eval(gamma, cont :: stack, gotten, ctx)
  //case LCLet(x, e, p) =>
  //val (nctx, fr) = fresh(ctx)
  //val l = subst(x, fr, p)
  //eval(gamma + (fr -> e), stack, l, nctx)
  //case LCTerminalOperation(x, op, y) =>
  //val cont1: Continuation = params1 => {
  //val cont2: Continuation = params2 => {
  //val expres = (params1.exp, params2.exp) match {
  //case (LCNumber(n), LCNumber(t)) =>
  //op match {
  //case Addition => n + t
  //case Subtraction => n - t
  //case Equallity => if (n == t) 1 else 0
  //case o => throw new Exception(s"didnt implement operator for $o between $n and $t")
  //}
  //case t2 => throw new Exception(s"expected numbers, found $t2")
  //}
  //continue(params2.gamma, params2.stack, LCNumber(expres), params2.ctx)
  //}
  //eval(params1.gamma, cont2 :: params1.stack, y, params1.ctx)
  //}
  //eval(gamma, cont1 :: stack, x, ctx)
  //case n: LCNumber => continue(gamma, stack, n, ctx)
  //}
  //}
}
