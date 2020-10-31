package ir

import cats.data._
import ir.IRSymbols._
import ir.LCLanguage._
import par.TokenTypes._

import scala.collection.SortedSet
import java.{util => ju}

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

  trait SymbolType {
    def ord: Int
  }

  case object FunctionSym extends SymbolType {
    def ord: Int = 0
  }

  case class TypeConstructorSym(dj: DisjointUnion) extends SymbolType {
    def ord: Int = 1
  }

  implicit object symTypeOrdering extends Ordering[SymbolType] {
    override def compare(x: SymbolType, y: SymbolType): Int =
      x.ord - y.ord
  }

  implicit object symOrdering extends Ordering[SymValue] {
    override def compare(x: (String, SymbolType), y: (String, SymbolType)): Int = {
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

  def suspend(exp: LCExp): LCExp =
    LCFunction("unit", LCName("unit"), exp)

  def unSuspend(exp: LCExp): LCExp =
    LCApplication(exp, LCRawCode("nullptr"))

  def evalPatternMatch(sm: SymbolMap)(pm: PatternMatch): LCExp = {
    import pm.{cases, expr}

    val du = sm
      .getOrElse(cases.head.typeConstructor,
                 throw new Exception(
                   s"failed to find type constructor for ${cases.head.typeConstructor} with cases ${sm.keys}"
                 ))
      .toList
      .collect { case (tcName, itemInScope: TypeConstructorSym) => tcName -> itemInScope }
      .toMap
      .getOrElse(
        cases.head.typeConstructor,
        throw new Exception(
          s"did not find any type constructors for ${cases.head.typeConstructor} with cases ${sm.keys}"
        )
      )

    val dj = du.dj

    val caseMap = cases.toList.map(x => (x.typeConstructor, x)).toMap

    val asLC = evalExpr(sm)(expr)

    dj.types.foldLeft(asLC) {
      case (accum, next) =>
        val name = next.name
        val item = caseMap
          .get(name)
          .orElse(caseMap.get("_"))
          .getOrElse(throw new Exception(s"did not find any match at ${name} or wildcard"))

        val params = item.bindings.foldLeft(buildFunBody(sm)(item.body)) {
          case (accum, next) =>
            LCFunction(next, LCName(next), accum)
        }

        LCApplication(accum, params)
    }
  }

  def evalExpr(sm: SymbolMap)(expr: Expression): LCExp = expr match {
    case InfixBuiltin(lhs, op, rhs) => {
      val lhEval = evalExpr(sm)(lhs)
      val rhEval = evalExpr(sm)(rhs)

      suspend(LCTerminalOperation(unSuspend(lhEval), op, unSuspend(rhEval)))
    }
    case ConstantInteger(n) => suspend(LCNumber(n))
    case ConstantStr(s)     => LCString(s.mkString)
    case Apply(f, e) =>
      val fName = LCName(f)

      def makeE(inner: LCExp): LCExp = e.foldLeft[LCExp](inner) {
        case (accum, next) => LCApplication(accum, evalExpr(sm)(next))
      }

      sm.get(f) match {
        case None => makeE(fName)
        case Some(ps) =>
          makeE(
            ps.collect { case (name, FunctionSym) => name }
              .toList
              .reverse
              .foldLeft[LCExp](fName) { case (accum, next) => LCApplication(accum, LCName(next)) }
          )
      }
    case If(expr, fst, snd) =>
      val cndApp = LCApplication(LCName(IF), evalExpr(sm)(expr))
      val fstApp = LCApplication(cndApp, buildFunBody(sm)(fst))
      LCApplication(fstApp, buildFunBody(sm)(snd))
    case p: PatternMatch => evalPatternMatch(sm)(p)
  }

  def buildFunBody(fm: SymbolMap)(body: FunctionBody): LCExp =
    buildDeclarations(fm)(body.children, evalExpr(fm)(body.`end`))

  def buildValueDeclaration(fm: SymbolMap)(vd: ValueDeclaration): Option[(String, LCExp)] = vd match {
    case FunDecl(name, params, body) =>
      val built = buildFunBody(fm)(body)
      val withParams = params.reverse.foldLeft(built) {
        case (accum, next) =>
          LCFunction(next.id, LCName(next.id), accum)
      }

      Some(name -> withParams)
    case LetDecl(name, expr) =>
      Some(name -> evalExpr(fm)(expr))
    case Import(_) => ???
    case Ignore    => None
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
    val fName = tt.name
    val params = tt.ids.zipWithIndex.map {
      case (tp, i) =>
        val parameterName = tp match {
          case ParensType(params) => params.name
          case TypeName(name)     => name
        }
        parameterName + s"_${i}"
    }

    val fLCName = LCName(fName)

    val patternHandler = params.reverse.foldLeft[LCExp](fLCName) {
      case (accum, next) =>
        LCApplication(accum, LCName(next))
    }

    val parameterization = allTags.reverse.foldLeft(patternHandler) {
      case (accum, next) =>
        LCFunction(next.name, LCName(next.name), accum)
    }

    val constructor = params.reverse.foldLeft(parameterization) {
      case (accum, next) =>
        LCFunction(next, LCName(next), accum)
    }

    (fName, constructor)
  }

  def buildTypeDeclaration(td: TypelevelDeclaration): List[(SymbolType, (String, LCExp))] = td match {
    case TypeDeclaration(_, _, expr) =>
      val out = expr.types.map(tt => buildTagType(expr.types)(tt))
      out.toList.map(x => (TypeConstructorSym(expr), x))
  }

  def buildDeclaration(fm: SymbolMap)(decl: Declaration): List[(SymbolType, (String, LCExp))] = decl match {
    case vd: ValueDeclaration     => buildValueDeclaration(fm)(vd).toList.map(x => (FunctionSym, x))
    case td: TypelevelDeclaration => buildTypeDeclaration(td)
    case Ignore                   => Nil
  }

  def buildDeclarations(fm: SymbolMap)(body: List[Declaration], expr: LCExp): LCExp = {
    val dis = declsInScope(body)
    val modFm: SymbolMap = fm ++ dis.map { case (x, _) => x -> dis }.toMap

    val definedDeclarations = body.flatMap(buildDeclaration(modFm))

    val typesDeclsFirst = definedDeclarations
      .sortBy(_._1)
      .collect { case (_, v) => v }

    typesDeclsFirst.foldLeft(expr) {
      case (innerExp, (name, functionBodyExp)) =>
        val aps: LCExp = dis.collect { case (x, FunctionSym) => x }.toList.foldLeft[LCExp](functionBodyExp) {
          case (accum, next) =>
            LCFunction(next, LCName(next), accum)
        }
        LCApplication(LCFunction(name, LCName(name), innerExp), aps)
    }
  }

  def vdInScope(vd: ValueDeclaration): SymbolSet = vd match {
    case FunDecl(name, _, _) => SortedSet((name, FunctionSym))
    case LetDecl(_, _)       => SortedSet.empty
    case Import(_)           => SortedSet.empty
    case Ignore              => SortedSet.empty
  }

  def tdInScope(td: TypelevelDeclaration): SymbolSet = td match {
    case TypeDeclaration(_, _, expr) =>
      SortedSet(expr.types.map(tt => (tt.name, TypeConstructorSym(expr))).toList: _*)
  }

  def declInScope(d: Declaration): SymbolSet = d match {
    case vd: ValueDeclaration     => vdInScope(vd)
    case td: TypelevelDeclaration => tdInScope(td)
    case Ignore                   => SortedSet.empty
  }

  def declsInScope(b: List[Declaration]): SymbolSet = b.foldLeft(SortedSet.empty[(String, SymbolType)]) {
    case (accum, next) =>
      accum ++ declInScope(next)
  }

  def applyStd(program: LCExp): LCExp = {
    // If
    val withIf = {
      val expName = "exp"
      val fstName = "fst"
      val sndName = "snd"
      val ifStatement = s"(${expName})(nullptr) ? (${fstName})(nullptr) : (${sndName})(nullptr)"
      val sndLayer = LCFunction(sndName, LCName(sndName), suspend(LCRawCode(ifStatement)))
      val fstLayer = LCFunction(fstName, LCName(fstName), sndLayer)
      val expLayer = LCFunction(expName, LCName(expName), fstLayer)
      LCApplication(LCFunction(IF, LCName(IF), program), expLayer)
    }

    withIf
  }

  def entrypoint(toplevelBody: List[Declaration]): LCExp = {
    def toNel(s: String) = s.toList match {
      case Nil     => ???
      case x :: xs => NonEmptyList(x, xs)
    }

    val xs = declsInScope(toplevelBody)
    val main = "main"
    val out = buildFunBody(Map.empty)(
      FunctionBody(toplevelBody,
                   Apply(main,
                         xs.toList.reverse
                           .collect { case (x, FunctionSym) => Apply(x, Nil) }))
    )
    applyStd(out)
  }
}
