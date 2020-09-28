package ir

import cats.Eval
import cats.data.NonEmptyList
import ir.IRSymbols._
import ir.LCLanguage._
import par.TokenTypes
import par.TokenTypes._
object LCTransform {

  // Returns a list of required parameters to realize the funciton
  def apply(f: Identifier, es: List[Expression])(ts: SymbolState): LiftedLambda[LCExp] = {
    /*
    f' = (λf.λg.λa.λb.g a b)
    g' = (λg.λf.λa.λb.f a b)

    f = f' f' g
    g = g' g' f

    f a b
    g a b
    */
    val id = f.dn

    def isClosure(depth: Depth, name: String): List[Closure] =
      if (depth < ts.depth) List(Closure(depth, name)) else Nil

    val letD: Option[(Depth, Eval[LiftedLambda[LCExp]])] = ts.let.get(id)
        .map(x => x.depth -> Eval.later(symbolizeExpression(x.t.value)(ts) wc isClosure(x.depth, x.t.varname.dn.name)))
    val funcD: Option[(Depth, Eval[LiftedLambda[LCExp]])] = ts.func.get(id)
      //.map(x => x.depth -> Eval.later(makeFunctionSymbol(x.t)(ts) wc isClosure(x.depth, x.t.varname.dn.name)))
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
      val nextSym = symbolizeExpression(next)(ts)
      LiftedLambda(LCApplication(accum.exp, nextSym.exp), accum.closures ++ nextSym.closures)
    }
  }

  def symbolizeExpression(exp: Expression)(ts: SymbolState): LiftedLambda[LCExp] = exp match {
    case InfixBuiltin(lhs, op, rhs) => {
      val lhEval = symbolizeExpression(lhs)(ts)
      val rhEval = symbolizeExpression(rhs)(ts)

      LiftedLambda(LCTerminalOperation(lhEval.exp, op, rhEval.exp), lhEval.closures ++ rhEval.closures)
    }
    case ConstantInteger(n) => LiftedLambda(LCNumber(n.dn.name.toInt), Nil)
    case ConstantStr(s) => LiftedLambda(LCString(s.mkString), Nil)
    case Apply(f, e) =>
      apply(f, e)(ts)
  }

  def symbolizeFunctionBody(children: List[Declaration], exp: Expression)(ts: SymbolState): LiftedLambda[LCExp] = {
    val (bindings, nt) = symbolizeDeclarations(children)(ts)
    val modT = ts combine nt
    bindings match {
      case Nil => symbolizeExpression(exp)(modT)
      case x :: xs =>
        val l = symbolizeExpression(exp)(modT)
        LiftedLambda(LCBody(NonEmptyList(x, xs), l.exp), l.closures)
    }
  }

  def makeFunctionSymbol(d: FunDecl)(ts: SymbolState): LiftedLambda[LCBody] = {
    val depthOne = ts.copy(depth = ts.depth + 1)
    val selfRef = depthOne combine SymbolState(func = Map(d.varname.dn -> Symbol[FunDecl](ts.depth, d)))
    val modT = d.params.foldLeft(selfRef) { case (accum, next) =>
      accum combine SymbolState(param=Map(next.id.dn.name -> Symbol(accum.depth, ParameterSymbol(next.id.dn.name))))
    }
    val x = d.body.fold(x => symbolizeFunctionBody(x.children, x.`end`)(modT), symbolizeFunctionBody(Nil, _)(modT))
    // Define actual parameters first
    val parameterized = d.params.reverse.foldLeft(x.exp) { case (accum, next) =>
      LCFunction(s"arg", LCName(next.id.dn.name), accum)
    }
    // Variables applicable from current level
    val (applicable, rest) = x.closures.partition(_.depth == ts.depth)
    val curriedPrime = applicable.foldLeft(parameterized){ case (accum, next) =>
      LCFunction(s"curry_${next.symbolName}", LCName(next.symbolName), accum)
    }
    val selfRefName = s"${d.varname.dn.name}_prime"
    val bindingName = LCName(selfRefName)
    val binding = LCBinding(bindingName, curriedPrime)
    val applied = applicable.reverse.foldLeft((bindingName): LCExp){ case (accum, next) =>
      val suffix = if (selfRef.func.contains(DeclarationName(next.symbolName))) {
        "_prime"
      } else ""
      LCApplication(accum, LCName(next.symbolName + suffix))
    }
    val nonPrime = LCName(s"${d.varname.dn.name}")
    val appliedNonPrime = LCBinding(nonPrime, applied)
    val body = LCBody(NonEmptyList(binding, List(appliedNonPrime)), nonPrime)
    LiftedLambda(body, rest)
  }

  def extractValueDeclName(vd: ValueDeclaration)(ts: SymbolState): (List[LCBinding], SymbolState) = vd match {
    case x@FunDecl(name, _, _) =>
      val bs = makeFunctionSymbol(x)(ts)

      bs.exp.bindings.toList -> SymbolState(func=Map(name.dn -> Symbol[FunDecl](ts.depth, x)))
    case x@LetDecl(name, _) =>
      List.empty -> SymbolState(let = Map(name.dn -> Symbol(ts.depth, x)))
    case Import(_) => ???
    case TokenTypes.Ignore =>
      List.empty -> SymbolState()
  }
/*
  def extractTypeDecl(vd: TypelevelDeclaration)(ts: SymbolState): SymbolState = vd match {
    case td@TypeDeclaration(_, _, expr) =>
      expr match {
        case DisjointUnion(_) => ??? //types.toList.map(tt => (tt.name.dn, (td, tt)))
      }
  }*/

  def symbolizeDeclaration(decl: Declaration)(ts: SymbolState): (List[LCBinding], SymbolState) = decl match {
    case decl: ValueDeclaration => extractValueDeclName(decl)(ts)
    case _: TypelevelDeclaration => ??? //extractTypeDecl(vd)(ts)
    case TokenTypes.Ignore => List.empty -> SymbolState()
  }

  def symbolizeDeclarations(body: List[Declaration])(ts: SymbolState): (List[LCBinding], SymbolState) = {
    body.foldLeft(List.empty[LCBinding] -> ts) { case ((accumBindings, accumTs), next) =>
      val (nBindings, nTs) = symbolizeDeclaration(next)(accumTs)
      (accumBindings ++ nBindings) -> (accumTs combine nTs)
    }
  }

  def transformEntry(toplevelBody: List[Declaration]): LCExp = {
    symbolizeFunctionBody(toplevelBody, Apply(NonEmptyList('m', List('a', 'i', 'n')), List.empty))(SymbolState()).exp
  }
}
