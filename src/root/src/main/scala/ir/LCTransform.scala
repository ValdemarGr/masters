package ir

import cats.data.NonEmptyList
import par.TokenTypes
import par.TokenTypes._
import LCLanguage._
import cats.{Eval, Monoid}

object LCTransform {

  case class DeclarationName(name: String)
  case class Symbol[T](
                      depth: Int,
                      t: T
                      )
  type Depth = Int

  type K = DeclarationName
  type LetTable = Map[K, Symbol[LetDecl]]

  type TypeV = (TypeDeclaration, TagType)
  type TypeDeclTable = Map[K, Symbol[TypeV]]

  type FunctionTable = Map[K, Symbol[FunDecl]]

  case class ParameterSymbol(
                            symbolName: String
                            )

  type ParameterTable = Map[String, Symbol[ParameterSymbol]]

  implicit val tsMonoid = new Monoid[TransformerState] {
    override def empty: TransformerState = TransformerState()

    override def combine(x: TransformerState, y: TransformerState): TransformerState =
      TransformerState(
        x.let ++ y.let,
        x.typeDecl ++ y.typeDecl,
        x.func ++ y.func,
        x.param ++ y.param,
        depth=Math.max(x.depth, y.depth)
      )
  }

  implicit class TSOps(ts: TransformerState)(implicit M: Monoid[TransformerState]) {
    def combine(that: TransformerState): TransformerState = M.combine(ts, that)
  }

  case class TransformerState(
                               let: LetTable = Map.empty,
                               typeDecl: TypeDeclTable = Map.empty,
                               func: FunctionTable = Map.empty,
                               param: ParameterTable = Map.empty,
                               depth: Depth = 0,
                             )

  implicit class NELDN(nel: NonEmptyList[Char]) {
    val dn = DeclarationName(nel.toList.mkString)
  }

  case class Closure(
                    depth: Depth,
                    symbolName: String
                    )
  case class LiftedLambda[E <: LCExp](
                               exp: E,
                               closures: List[Closure]
                               ) {
    // withClosure
    def wc(c: List[Closure]): LiftedLambda[LCExp] = copy(closures=closures ++ c)
  }

  // Returns a list of required parameters to realize the funciton
  def apply(f: Identifier, es: List[Expression])(ts: TransformerState): LiftedLambda[LCExp] = {
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

  def symbolizeExpression(exp: Expression)(ts: TransformerState): LiftedLambda[LCExp] = exp match {
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

  def symbolizeFunctionBody(children: List[Declaration], exp: Expression)(ts: TransformerState): LiftedLambda[LCExp] = {
    val (bindings, nt) = symbolizeDeclarations(children)(ts)
    val modT = ts combine nt
    bindings match {
      case Nil => symbolizeExpression(exp)(modT)
      case x :: xs =>
        val l = symbolizeExpression(exp)(modT)
        LiftedLambda(LCBody(NonEmptyList(x, xs), l.exp), l.closures)
    }
  }

  def makeFunctionSymbol(d: FunDecl)(ts: TransformerState): LiftedLambda[LCBody] = {
    val depthOne = ts.copy(depth = ts.depth + 1)
    val selfRef = depthOne combine TransformerState(func = Map(d.varname.dn -> Symbol[FunDecl](ts.depth, d)))
    val modT = d.params.foldLeft(selfRef) { case (accum, next) =>
      accum combine TransformerState(param=Map(next.id.dn.name -> Symbol(accum.depth, ParameterSymbol(next.id.dn.name))))
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

  def extractValueDeclName(vd: ValueDeclaration)(ts: TransformerState): (List[LCBinding], TransformerState) = vd match {
    case x@FunDecl(name, _, _) =>
      val bs = makeFunctionSymbol(x)(ts)

      bs.exp.bindings.toList -> TransformerState(func=Map(name.dn -> Symbol[FunDecl](ts.depth, x)))
    case x@LetDecl(name, _) =>
      List.empty -> TransformerState(let = Map(name.dn -> Symbol(ts.depth, x)))
    case FunctionParam(_) => ???
    case Import(_) => ???
    case TokenTypes.Ignore =>
      List.empty -> TransformerState()
  }
/*
  def extractTypeDecl(vd: TypelevelDeclaration)(ts: TransformerState): TransformerState = vd match {
    case td@TypeDeclaration(_, _, expr) =>
      expr match {
        case DisjointUnion(_) => ??? //types.toList.map(tt => (tt.name.dn, (td, tt)))
      }
  }*/

  def symbolizeDeclaration(decl: Declaration)(ts: TransformerState): (List[LCBinding], TransformerState) = decl match {
    case decl: ValueDeclaration => extractValueDeclName(decl)(ts)
    case _: TypelevelDeclaration => ??? //extractTypeDecl(vd)(ts)
    case TokenTypes.Ignore => List.empty -> TransformerState()
  }

  def symbolizeDeclarations(body: List[Declaration])(ts: TransformerState): (List[LCBinding], TransformerState) = {
    body.foldLeft(List.empty[LCBinding] -> ts) { case ((accumBindings, accumTs), next) =>
      val (nBindings, nTs) = symbolizeDeclaration(next)(accumTs)
      (accumBindings ++ nBindings) -> (accumTs combine nTs)
    }
  }

  def transformEntry(toplevelBody: List[Declaration]): LCExp = {
    symbolizeFunctionBody(toplevelBody, Apply(NonEmptyList('m', List('a', 'i', 'n')), List.empty))(TransformerState()).exp
  }
  /*
    def transformFun(b: List[Declaration], e: Expression)
                    (ts: TransformerState): LCExp = {
      val nt = symbolizeDeclarations(b)(ts)
      transformExpr(e)(nt)
    }

    def application(v: ValueDeclaration, args: List[LCExp])
                   (t: TransformerState): LCExp = {
      val applyParams = (innerExpression: LCExp) => args.foldLeft(innerExpression) { case (accum, next) =>
        LCApplication(next, accum)
      }
      v match {
        case FunDecl(_, pars, body) => {
          val stP: ParameterTable = pars.map { a =>
            val id = a.id.dn
            id.name -> Symbol[ParameterSymbol](t.depth, ParameterSymbol(id.name))
          }.toMap
          val modT = t combine TransformerState(param = stP)
          transformFunBody(body)(modT)
        }
        case LetDecl(_, value) => transformExpr(value)(t)
        case FunctionParam(id) => applyParams(LCName(id.dn.name))
        case Import(_) => ???
        case TokenTypes.Ignore => ???
      }

    }

    def applyTypeConstructor(t: TypeDeclaration, tt: TagType): LCExp =
      t.expr match {
        case DisjointUnion(types) =>
          val pos = types
            .zipWithIndex
            .find { case (x, _) => x == tt }
            .map { case (_, i) => i }
            .getOrElse(throw new Exception(s"failed to find ${tt} in ${t}"))
          println(pos)
          ???
      }

    def transformExpr(e: Expression)
                     (t: TransformerState): LCExp = e match {
      case InfixBuiltin(lhs, op, rhs) => {
        val fst = LCName("fst")
        val snd = LCName("snd")
        val f = LCFunction("infix1", fst, LCFunction("infix2", snd, LCTerminalOperation(fst, op, snd)))
        val lhEval = transformExpr(lhs)(t)
        val rhEval = transformExpr(rhs)(t)
        LCApplication(LCApplication(f, lhEval), rhEval)
      }
      case FunctionBody(children, end) => transformFun(children, end)(t)
      case ConstantInteger(n) => LCNumber(n.dn.name.toInt)
      case ConstantStr(s) => LCString(s.mkString)
      case Apply(f, e) =>
        ???

      // Either its an already defined function, so we bind the parameters effectively unpacking it
      val eo = et.get(f.dn)
      // Or it is a symbol, eg a reference
      val so = st.get(f.dn).map(x => application(x, e.map(x => transformExpr(x)(t)))(t))
      // Or it is a type constructor
      val to = td.get(f.dn).map{ case (td, tt) => applyTypeConstructor(td, tt) }
      Seq(eo, so, to).flatten.headOption match {
        case Some(x) => x
        case None => throw new Exception(s"failed to find symbol ${f.dn.name}")
      }
    }

    def transformFunBody(b: Either[FunctionBody, Expression])
                        (t: TransformerState): LCExp = {
      val modT = t.copy(depth = t.depth + 1)
      b match {
        case Left(value) => transformFun(value.children, value.`end`)(modT)
        case Right(value) => transformExpr(value)(modT)
      }
    }*/
}
