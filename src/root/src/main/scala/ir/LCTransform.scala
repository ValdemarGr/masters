package ir

import cats.data.NonEmptyList
import ir.IRSymbols._
import ir.LCLanguage._
import par.TokenTypes._

import scala.collection.SortedSet

/*
     (λf'.
       (λg'.
         (λf.
           (λg.
             //main
             f (2)
           ) (g' g' f')
         ) (f' f' g')
       ) (λg.λf.(λa.f f g a))
     ) (λf.λg.(λa.g g f a))
     ---
     parsing body
       1) find all fun and let decls and inject them, special case for functions:
           (LCApplication(LCFunction(name+"_prime", parseNext(...)), parseFunBody(...)))
           and accumulate name+"_prime" functions into some sorted list
       2) traverse functions_prime in above list and apply "this level's" curried values
*/
object LCTransform {
  val IF = "IF"

  trait SymbolType { def ord: Int}
  case object FunctionSym extends SymbolType { def ord: Int = 0}
  case object TypeConstructorSym extends SymbolType { def ord: Int = 1}

  implicit object symTypeOrdering extends Ordering[SymbolType] {
    override def compare(x: SymbolType, y: SymbolType): Int =
      x.ord - y.ord
  }

  implicit object symOrdering extends Ordering[SymValue] {
    override def compare(x: (String, SymbolType), y: (String, SymbolType)): Int =  {
      val symOrd = implicitly[Ordering[SymbolType]].compare(x._2, y._2)
      if (symOrd != 0) {
        symOrd
      } else {
        implicitly[Ordering[String]].compare(x._1, y._1)
      }
    }
  }

  type SymValue = (String, SymbolType)
  type SymbolSet = SortedSet[SymValue]
  type SymbolMap = Map[String, SymbolSet]

  def evalExpr(sm: SymbolMap)(expr: Expression): LCExp = expr match {
    case InfixBuiltin(lhs, op, rhs) => {
      val lhEval = evalExpr(sm)(lhs)
      val rhEval = evalExpr(sm)(rhs)

      LCTerminalOperation(lhEval, op, rhEval)
    }
    case ConstantInteger(n) => LCNumber(n)
    case ConstantStr(s) => LCString(s.mkString)
    case Apply(f, e) =>
      val fName = LCName(f.str)
      def makeE(inner: LCExp): LCExp = e.foldLeft[LCExp](inner){ case (accum, next) => LCApplication(accum, evalExpr(sm)(next)) }
      sm.get(f.str) match {
        case None => makeE(fName)
        case Some(ps) => makeE(ps
          .collect{ case (name, FunctionSym) => name }
          .toList
          .reverse
          .foldLeft[LCExp](fName){ case (accum, next) => LCApplication(accum, LCName(next)) })
      }
    case If(expr, fst, snd) =>
      val cndApp = LCApplication(LCName(IF), evalExpr(sm)(expr))
      val fstApp = LCApplication(cndApp, buildFunBody(sm)(fst))
      LCApplication(fstApp, buildFunBody(sm)(snd))
  }

  def buildFunBody(fm: SymbolMap)(body: FunctionBody): LCExp =
    buildDeclarations(fm)(body.children, evalExpr(fm)(body.`end`))

  def buildValueDeclaration(fm: SymbolMap)(vd: ValueDeclaration): Option[(String, LCExp)] = vd match {
    case FunDecl(name, params, body) =>
      val built = buildFunBody(fm)(body)
      val withParams = params.reverse.foldLeft(built){ case (accum, next) =>
        LCFunction(next.id.str, LCName(next.id.str), accum)
      }
      Some(name.str -> withParams)
    case LetDecl(name, expr) =>
      Some(name.str -> evalExpr(fm)(expr))
    case Import(_) => ???
    case Ignore => None
  }

  /*
    Maybe
    data Maybe a = Just a | Nothing
    maybe a: (a -> b) -> b -> b

    just a = (onJust, onNothing) => onJust(a)
    nothing = (onJust, onNothing) => onNothing()

    just = λa.(λj.λn.j a)
    nothing = (λj.λn.n)

    test = just 4
    test (λa.a + 5) (5) // 9

    test = nothing
    test (λa.a + 5) (5) // 5

    List
    data List a = Nil | Cons a (List a)

    nil = (onNil, onCons) => onNil()
    cons x xs = (onNil, onCons) => onCons(x, xs)

    test = nil
    test (5) (λx.λxs.x + 4) // 5

    test = cons(4, nil)
    test (5) (λx.λxs.x + 4) // 9

    test = cons(4, cons(10, nil))
    test (5) (λx.λxs.xs (5) (λx2.λxs2. x + x2)) // 10+4=14

    test = cons(4, cons(10, const(2, nil)))
    fr = (λf.λl. l (0) (λx.λxs.x + (f f xs)))
    fl = (λf.λa.λl. l (0) (λx.λxs.f f (x + a) xs))
    fr fr test // 16

    to encode ADT's
      1) run through all type declarations
      2) encode the constructors as functions
  */
  //One constructor for each

  def buildTagType(allTags: NonEmptyList[TagType])(tt: TagType): (String, LCExp) = {
    val fName = tt.name.str
    val params = tt.ids.zipWithIndex.map{ case (tp, i) =>
      val parameterName = tp match {
        case ParensType(params) => params.name.str
        case TypeName(name) => name.str
      }
      parameterName + s"_${i}"
    }

    val fLCName = LCName(fName)

    val patternHandler = params.reverse.foldLeft[LCExp](fLCName) { case (accum, next) =>
      LCApplication(accum, LCName(next))
    }

    val parameterization = allTags.reverse.foldLeft(patternHandler) { case (accum, next) =>
      LCFunction(next.name.str, LCName(next.name.str), accum)
    }

    val constructor = params.reverse.foldLeft(parameterization) { case (accum, next) =>
      LCFunction(next, LCName(next), accum)
    }

    (fName, constructor)
  }

  def buildTypeDeclaration(td: TypelevelDeclaration): NonEmptyList[(String, LCExp)] = td match {
    case TypeDeclaration(_, _, expr) =>
      expr.types.map(tt => buildTagType(expr.types)(tt))
  }

  def buildDeclaration(fm: SymbolMap)(decl: Declaration): List[(SymbolType, (String, LCExp))] = decl match {
    case vd: ValueDeclaration => buildValueDeclaration(fm)(vd).toList map (x => (FunctionSym, x))
    case td: TypelevelDeclaration => buildTypeDeclaration(td).toList map (x => (TypeConstructorSym, x))
    case Ignore => Nil
  }

  def buildDeclarations(fm: SymbolMap)(body: List[Declaration], expr: LCExp): LCExp = {
    val dis = declsInScope(body)
    val modFm: SymbolMap = fm ++ dis
      .map { case (x, _) => x -> dis }
      .toMap

    val definedDeclarations = body.flatMap(buildDeclaration(modFm))

    val typesDeclsFirst =  definedDeclarations.sortBy(_._1)
      .collect{ case (_, v) => v }

    typesDeclsFirst.foldLeft(expr) { case (innerExp, (name, functionBodyExp)) =>
      val aps: LCExp = dis.collect{ case (x, FunctionSym) => x }.toList.foldLeft[LCExp](functionBodyExp) { case (accum, next) =>
        LCFunction(next, LCName(next), accum)
      }
      LCApplication(LCFunction(name, LCName(name), innerExp), aps)
    }
  }

  def vdInScope(vd: ValueDeclaration): SymbolSet = vd match {
    case FunDecl(name, _, _) => SortedSet((name.str, FunctionSym))
    case LetDecl(name, _) => SortedSet((name.str, FunctionSym))
    case Import(_) => SortedSet.empty
    case Ignore => SortedSet.empty
  }

  def tdInScope(td: TypelevelDeclaration): SymbolSet = td match {
    case TypeDeclaration(_, _, expr) => SortedSet(expr.types.map(tt => (tt.name.str, TypeConstructorSym)).toList: _*)
  }

  def declInScope(d: Declaration): SymbolSet = d match {
    case vd: ValueDeclaration => vdInScope(vd)
    case td: TypelevelDeclaration => tdInScope(td)
    case Ignore => SortedSet.empty
  }

  def declsInScope(b: List[Declaration]): SymbolSet = b.foldLeft(SortedSet.empty[(String, SymbolType)]){ case (accum, next) =>
    accum ++ declInScope(next)
  }

  def applyStd(program: LCExp): LCExp = {
    // If
    val withIf = {
      val expName = "exp"
      val fstName = "fst"
      val sndName = "snd"
      val ifStatement = s"(${expName}) ? (${fstName}) : (${sndName})"
      val sndLayer = LCFunction(sndName, LCName(sndName), LCRawCode(ifStatement))
      val fstLayer = LCFunction(fstName, LCName(fstName), sndLayer)
      val expLayer = LCFunction(expName, LCName(expName), fstLayer)
      LCApplication(LCFunction(IF, LCName(IF), program), expLayer)
    }

    withIf
  }

  def entrypoint(toplevelBody: List[Declaration]): LCExp = {
    def toNel(s: String) = s.toList match {
      case Nil => ???
      case x :: xs => NonEmptyList(x, xs)
    }
    val xs = declsInScope(toplevelBody)
    val main = NonEmptyList('m', List('a', 'i', 'n'))
    val out = buildFunBody(Map.empty)(FunctionBody(toplevelBody, Apply(main,
      xs
        .toList
        .reverse
        .collect{ case (x, FunctionSym) => Apply(toNel(x), Nil) }
    )))
    applyStd(out)
  }

  /*def apply(f: Identifier, es: List[Expression])(ts: SymbolState): LiftedLambda[LCExp] = {
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
  }*/
}
