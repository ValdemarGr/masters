package ir

import cats.data.NonEmptyList
import cats.{Eval, Monoid}
import par.TokenTypes._

object LCGeneration {
  import IRSymbols._
  import LCLanguage._

  case class PrimeFun(
                       fd: FunDecl,
                       binding: LCBinding,
                       applicable: List[Closure],
                       rest: List[Closure]
                     )

  type R = (List[PrimeFun], SymbolState)
  implicit val M: Monoid[R] = new Monoid[R] {
    def empty: (List[PrimeFun], SymbolState) =
      (List.empty, SymbolState())
    def combine(x: (List[PrimeFun], SymbolState), y: (List[PrimeFun], SymbolState)): (List[PrimeFun], SymbolState) =
      (x._1 ++ y._1, x._2 combine y._2)
  }

  def getPrimeFuncs(decls: List[Declaration])(ss: SymbolState): (List[PrimeFun], SymbolState) =
    unwrapDecls[R](decls)(ss) { fd =>
      val f = genPrimeFun(fd)(ss)
      List(f) -> SymbolState(unappliedFunc=Map(fd.varname.dn -> Symbol[UnappliedFunc](ss.depth, UnappliedFunc(fd, f.applicable))))
    }

  def genFuncSymbols(decls: List[Declaration])(ss: SymbolState): SymbolState =
    unwrapDecls[SymbolState](decls)(ss) { fd =>
      SymbolState(func=Map(fd.varname.dn -> Symbol[FunDecl](ss.depth, fd)))
    }

  def genFunBody

  def apply(f: Identifier, es: List[Expression])(ts: SymbolState): LiftedLambda[LCExp] = {
    val id = f.dn

    def isClosure(depth: Depth, name: String): List[Closure] =
      if (depth < ts.depth) List(Closure(depth, name)) else Nil

    val letD: Option[(Depth, Eval[LiftedLambda[LCExp]])] = ts.let.get(id)
      .map(x => x.depth -> Eval.later(genExpression(x.t.value)(ts) wc isClosure(x.depth, x.t.varname.dn.name)))
    val funcD: Option[(Depth, Eval[LiftedLambda[LCExp]])] = ts.func.get(id)
      .map(x => x.depth -> Eval.later(LiftedLambda(LCName(x.t.varname.dn.name), isClosure(x.depth, x.t.varname.dn.name))))
    val paramD: Option[(Depth, Eval[LiftedLambda[LCExp]])] = ts.param.get(id.name)
      .map(x => x.depth -> Eval.later(LiftedLambda(LCName(x.t.symbolName), isClosure(x.depth, x.t.symbolName))))
    val tpeD: Option[(Depth, Eval[LiftedLambda[LCExp]])] = ts.typeDecl.get(id).map(_ => ???)

    val t = List(funcD, letD, paramD, tpeD)
      .flatten
      .sortBy{ case (d, _) => d }
      .headOption
      .map{ case (_, x) => x.value }

    val out = t.getOrElse(throw new Exception(s"Failed to find apply id for ${id.name}"))

    es.foldLeft(out){ case (accum, next) =>
      val nextSym = genExpression(next)(ts)
      LiftedLambda(LCApplication(accum.exp, nextSym.exp), accum.closures ++ nextSym.closures)
    }
  }

  def genExpression(e: Expression)(ss: SymbolState): LiftedLambda[LCExp] = e match {
    case InfixBuiltin(lhs, op, rhs) => {
      val lhEval = genExpression(lhs)(ss)
      val rhEval = genExpression(rhs)(ss)

      LiftedLambda(LCTerminalOperation(lhEval.exp, op, rhEval.exp), lhEval.closures ++ rhEval.closures)
    }
    case ConstantInteger(n) => LiftedLambda(LCNumber(n.dn.name.toInt), Nil)
    case ConstantStr(s) => LiftedLambda(LCString(s.mkString), Nil)
    case Apply(f, e) =>
      apply(f, e)(ss)
  }

  def bindPrimes(primes: List[PrimeFun])(ss: SymbolState) = primes.map{ prime =>
    prime.applicable.reverse.foldLeft(???) { case (accum, next) =>
      ss.unappliedFunc.get(next.symbolName).orElse(ss.let.get())

    }
    LCBinding(prime.fd.varname.str, )
  }

  def genFunctionBody(children: List[Declaration], exp: Expression)(ss: SymbolState): LiftedLambda[LCExp] = {
    val newSs = genFuncSymbols(children)(ss)
    val (primes, modT) = getPrimeFuncs(children)(newSs)
    val bindings: List[LCBinding] = primes.map(_.binding)
    bindings match {
      case Nil => genExpression(exp)(modT)
      case x :: xs =>
        val l = genExpression(exp)(modT)
        LiftedLambda(LCBody(NonEmptyList(x, xs), l.exp), l.closures ++ primes.flatMap(_.rest))
    }
  }

  def genPrimeFun(d: FunDecl)(ss: SymbolState): PrimeFun = {
    val depthOne = ss.copy(depth = ss.depth + 1)
    val selfRef = depthOne combine SymbolState(func = Map(d.varname.dn -> Symbol[FunDecl](ss.depth, d)))
    val modT = d.params.foldLeft(selfRef) { case (accum, next) =>
      accum combine SymbolState(param=Map(next.id.dn.name -> Symbol(accum.depth, ParameterSymbol(next.id.dn.name))))
    }
    val x = d.body.fold(x => genFunctionBody(x.children, x.`end`)(modT), genFunctionBody(Nil, _)(modT))
    // Define actual parameters first
    val parameterized = d.params.reverse.foldLeft(x.exp) { case (accum, next) =>
      LCFunction(s"arg", LCName(next.id.dn.name), accum)
    }
    // Variables applicable from current level
    val (applicable, rest) = x.closures.partition(_.depth == ss.depth)
    val curriedPrime = applicable.foldLeft(parameterized){ case (accum, next) =>
      LCFunction(s"curry_${next.symbolName}", LCName(next.symbolName), accum)
    }
    val selfRefName = s"${d.varname.dn.name}_prime"
    val bindingName = LCName(selfRefName)
    val binding = LCBinding(bindingName, curriedPrime)
    PrimeFun(d, binding, applicable, rest)
  }
}
