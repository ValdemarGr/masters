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
  type Variable = String
  type Heap = Map[Variable, LCExp]
  type Timestamp = Int

  class DualHeap(
    val live: Map[Variable, (LCExp, Timestamp)],
    val idle: Map[Variable, (LCExp, Timestamp)]
  ) {
    def size = live.size + idle.size
    def get(name: Variable): (LCExp, Timestamp) = 
      (live.get(name), idle.get(name)) match {
        case (Some((e1, t1)), Some((e2, t2))) =>
          if (t1 > t2) (e1, t1)
          else (e2, t2)
        case (o1, o2) =>
          o1.orElse(o2).getOrElse(throw new Exception(s"failed to find $name in dual heap"))
      }
    def insert(name: Variable, value: LCExp, ctx: Context): (DualHeap, Context) = {
      val nv = (name, (value, ctx.nextLamport))
      val nh = new DualHeap(live + nv, idle)
      (nh, ctx.copy(nextLamport = ctx.nextLamport + 1))
    }
    def swap = new DualHeap(idle, live)
  }

  final case class ContinuationParams(
    gamma: Heap,
    stack: ContStack,
    exp: LCExp,
    ctx: Context
  )
  type Continuation = ContinuationParams => LCExp
  type ContStack = List[Continuation]
  final case class Context(freshNum: Int, nextLamport: Int)

  val charRange = 'a'.toInt to 'z'.toInt toArray
  def fresh(ctx: Context): (Context, String) = {
    val fst = charRange(ctx.freshNum % charRange.length).toChar.toString
    val suf = (ctx.freshNum / charRange.length).toString
    val newName = fst + suf
    (ctx.copy(freshNum = ctx.freshNum + 1), newName)
  }

  def subst(f: Variable, t: Variable, exp: LCExp): LCExp = exp match {
    case LCFunction(m, x, e) =>
      if (x.name == f) exp
      else LCFunction(m, x, subst(f, t, e))
    case LCName(x) =>
      if (x == f) LCName(t)
      else LCName(x)
    case LCApplication(fst, snd) =>
      LCApplication(subst(f, t, fst), subst(f, t, snd))
    case LCTerminalOperation(lh, op, rh) =>
      LCTerminalOperation(subst(f, t, lh), op, subst(f, t, rh))
    case LCIf(exp, fst, snd) =>
      LCIf(subst(f, t, exp), subst(f, t, fst), subst(f, t, snd))
    case LCLet(name, exp, in) =>
      if (name == f) LCLet(name, exp, in)
      else LCLet(name, subst(f, t, exp), subst(f, t, in))
    case x => x
  }

  //def continue(gamma: Heap, stack: ContStack, exp: LCExp, ctx: Context): LCExp = stack match {
  //case Nil => exp
  //case x :: xs => x(ContinuationParams(gamma, xs, exp, ctx))
  //}

  def run(program: LCExp): LCExp = {
    val gamma: DualHeap = new DualHeap(Map.empty, Map.empty)
    val stack: ContStack = List.empty
    val ctx = Context(1, 1)
    val (heapout, out, _, _, gc) = eval(gamma, program, Nil, ctx, None).value
    println(s"final heap was size ${heapout.size}")
        //import scala.concurrent.duration._
    //val futcomp = Await.result(gc.get, 10.second)
    //println(s"future completed with $futcomp")
    out
  }

  type N = List[LCExp]

  def freeVars(e: LCExp): Set[String] = e match {
    case LCName(name) => Set(name)
    case LCApplication(l, p) =>
      freeVars(l).union(freeVars(p))
    case LCIf(e, f, s) => freeVars(e).union(freeVars(f)).union(freeVars(s))
    case LCFunction(_, x, e) =>
      freeVars(e) - x.name
    case LCLet(x, e, p) =>
      (freeVars(e).union(freeVars(p))) - x
    case LCNumber(_)                  => Set.empty
    case LCTerminalOperation(l, _, r) => freeVars(l).union(freeVars(r))
  }

  def syncGarbageCollect(gamma: Heap, n: N, accum: Heap): Heap = n match {
    case Nil => accum
    case x :: xs =>
      val frees = freeVars(x)
      val (toKeep, rest) = gamma.partition { case (k, _) => frees.contains(k) }
      syncGarbageCollect(rest, toKeep.values.toList ++ xs, toKeep ++ accum)
  }

  def parallelGarbageCollect(dh: DualHeap, n: N, accum: Map[Variable, (LCExp, Timestamp)]): Map[Variable, (LCExp, Timestamp)] = n match {
    case Nil => accum
    case x :: xs =>
      val frees  = freeVars(x)
      val toKeepIdle = dh.idle.filter{ case (k, _) => frees.contains(k) }
      val toKeepLive = dh.live.filter{ case (k, _) => frees.contains(k) }
      def newest(one: Map[Variable, (LCExp, Timestamp)], two: Map[Variable, (LCExp, Timestamp)]) = {
        type T = Either[(Variable, (LCExp, Timestamp)), (Variable, (LCExp, Timestamp))]
        val setEither: Set[T] = (one.keySet ++ two.keySet).map{ k =>
          (one.get(k), two.get(k)) match {
            case (Some((e1, t1)), Some((e2, t2))) =>
              if (t1 > t2) Left((k, (e1, t1)))
              else Right((k, (e2, t2)))
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
        dh.live.filter(x => !n.contains(x)),
        dh.idle.filter(x => !n.contains(x)),
      ), r.map(_._1) ++ xs, newIdle ++ accum)
  }

  type GCFuture = Option[Future[Map[Variable, (LCExp, Timestamp)]]]
  val sync = "sync"
  val par = "parallel"
  val none = "none"
  val gcStrategy = par
  def eval(he: DualHeap, exp: LCExp, ns: N, ctx: Context, gf: GCFuture): Eval[(DualHeap, LCExp, N, Context, GCFuture)] = {
    //println(s"runnig with $gamma $stack $exp $ctx")
    val (gamma, gcFuture) = if (math.random() < 0.1) {
      if (gcStrategy == sync) {
        val newHeap = syncGarbageCollect(he.live.mapValues(_._1), exp :: ns, Map.empty)
        (new DualHeap(newHeap.mapValues(e => (e, 0)), Map.empty), None)
      } else if (gcStrategy == none) {
        (he, None)
      } else {
        val (newHe, futGc) = gf match {
          case None => 
            val swapped = he.swap
            (swapped, Future(parallelGarbageCollect(swapped, exp :: ns, Map.empty)))
          case Some(value) => 
            (he, value)
        }
        if (futGc.isCompleted) {
          import scala.concurrent.duration._
          val newIdle = Await.result(futGc, 1.second)
          println(s"gc generated ${newIdle.size}, current has ${newHe.idle.size}")
          val dhprime = new DualHeap(newHe.live, newIdle)
          (dhprime, None)
        } else {
          (newHe, Some(futGc))
        }
      }
    } else (he, gf)
    exp match {
      case LCIf(e, fst, snd) =>
        Eval.defer(eval(gamma, e, fst :: (snd :: ns), ctx, gcFuture)).flatMap {
          case (sigma, LCNumber(n), ns, ctx, gcFuture) =>
            val next = if (n == 1) fst else snd
            Eval.defer(eval(sigma, next, ns.drop(2), ctx, gcFuture))
          case _ => ???
        }
      case e: LCFunction => Eval.later((gamma, e, ns, ctx, gcFuture))
      case LCApplication(l, p) =>
        p match {
          case LCName(name) =>
            Eval.defer(eval(gamma, l, p :: ns, ctx, gcFuture)).flatMap {
              case (sigma, LCFunction(_, x, e), ns, ctx, gcFuture) =>
                Eval.defer(eval(sigma, subst(x.name, name, e), ns.drop(1), ctx, gcFuture))
          case _ => ???
            }
          case other =>
            val (nctx, fr) = fresh(ctx)
            val bound = LCLet(fr, p, LCApplication(l, LCName(fr)))
            Eval.defer(eval(gamma, bound, ns, nctx, gcFuture))
        }
      case LCName(name) =>
        val (gotten, _) = gamma.get(name)
        Eval.defer(eval(gamma, gotten, ns, ctx, gcFuture)).flatMap {
          case (sigma, e, ns, ctx, gcFuture) =>
            val (newheap, nctx) = sigma.insert(name, e, ctx)
            Eval.later((newheap, e, ns, nctx, gcFuture))
        }
      case LCLet(x, e, p) =>
        val (nctx, fr) = fresh(ctx)
        val l = subst(x, fr, p)
        val (newheap, ctx2) = gamma.insert(fr, e, nctx)
        Eval.defer(eval(newheap, l, ns, ctx2, gcFuture))
      case LCTerminalOperation(x, op, y) =>
        Eval.defer(eval(gamma, x, y :: ns, ctx, gcFuture)).flatMap {
          case (theta, LCNumber(n), ns, ctx1, gcFuture) =>
            Eval.defer(eval(theta, y, ns.drop(1), ctx1, gcFuture)).flatMap {
              case (sigma, LCNumber(t), ns, ctx2, gcFuture) =>
                val expres =
                  op match {
                    case Addition    => n + t
                    case Subtraction => n - t
                    case Equallity   => if (n == t) 1 else 0
                    case o           => throw new Exception(s"didnt implement operator for $o between $n and $t")
                  }
                Eval.later((sigma, LCNumber(expres), ns, ctx2, gcFuture))
          case _ => ???
            }
          case _ => ???
        }
      case n: LCNumber => Eval.later((gamma, n, ns, ctx, gcFuture))
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
