package ir

import cats.data.NonEmptyList
import par.TokenTypes
import par.TokenTypes._
import LCLanguage._
import cats.Monoid
import cats.Eval

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

  type LCExpTable = Map[String, Symbol[LCExp]]

  implicit val tsMonoid = new Monoid[TransformerState] {
    override def empty: TransformerState = TransformerState()

    override def combine(x: TransformerState, y: TransformerState): TransformerState =
      TransformerState(
        x.let ++ y.let,
        x.typeDecl ++ y.typeDecl,
        x.func ++ y.func,
        x.exp ++ y.exp,
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
                               exp: LCExpTable = Map.empty,
                               depth: Depth = 0,
                             )

  implicit class NELDN(nel: NonEmptyList[Char]) {
    val dn = DeclarationName(nel.toList.mkString)
  }

  // Returns a list of required parameters to realize the funciton
  def apply(f: Identifier, e: List[Expression])(ts: TransformerState): (LCExp, List[String]) = {
    /*
    f' = (λf.λg.λa.λb.g a b)
    g' = (λg.λf.λa.λb.f a b)

    f = f' f' g
    g = g' g' f

    f a b
    g a b
    */
    val id = f.dn
    def free(d: Depth)(inline: => LCExp)(closure: => LCExp): (Depth, Eval[(LCExp, List[String])]) =
      d -> Eval.later {
        if (d > ts.depth) {
          LCFunction("closure", LCName(id.name), closure) -> List(id.name)
        } else {
          inline -> List.empty
        }
      }

    val letD: Option[(Depth, Eval[(LCExp, List[String])])] = ts.let.get(id)
      .map(x => free(x.depth)(symbolizeExpression(x.t.value)(ts))(???))
    val funcD: Option[(Depth, Eval[(LCExp, List[String])])] = ts.func.get(id).map(x => free(x.depth)(???)(???))
    val expD: Option[(Depth, Eval[(LCExp, List[String])])] = ts.exp.get(id.name).map(x => free(x.depth)(???)(???))
    val tpeD: Option[(Depth, Eval[(LCExp, List[String])])] = (ts.typeDecl.get(id).map(_ => ???): Option[Symbol[TypeV]]).map(x => free(x.depth, ???, ???))

    val o = List(funcD, letD, expD, tpeD)
      .flatten
      .sortBy{ case (d, _) => d }
      .headOption
      .map{ case (_, x) => x.value }
    o.getOrElse(throw new Exception(s"Failed to find apply id for ${id.name}"))
  }

  def symbolizeExpression(exp: Expression)(ts: TransformerState): LCExp = exp match {
    case InfixBuiltin(lhs, op, rhs) => {
      val fst = LCName("fst")
      val snd = LCName("snd")
      val f = LCFunction("infix1", fst, LCFunction("infix2", snd, LCTerminalOperation(fst, op, snd)))
      val lhEval = transformExpr(lhs)(ts)
      val rhEval = transformExpr(rhs)(ts)
      LCApplication(LCApplication(f, lhEval), rhEval)
    }
    case ConstantInteger(n) => LCNumber(n.dn.name.toInt)
    case ConstantStr(s) => LCString(s.mkString)
    case Apply(f, e) =>
      apply(f, e)(ts)
      ???
  }

  def symbolizeFunctionBody(children: List[Declaration], exp: Expression)(ts: TransformerState): LCExp = {
    val modT = ts combine symbolizeDeclarations(children)(ts)
    symbolizeExpression(exp)(modT)
  }

  def makeFunctionSymbol(d: FunDecl)(ts: TransformerState): FunctionSymbol = {
    val x = d.body.fold(x => symbolizeFunctionBody(x.children, x.`end`)(ts), symbolizeFunctionBody(List.empty, _)(ts))
    LCBinding(s"${d.varname.dn.name}_prime", )
  }

  def extractValueDeclName(vd: ValueDeclaration)(ts: TransformerState): TransformerState = vd match {
    case x@FunDecl(name, _, _) =>
      TransformerState(func = Map(name.dn -> Symbol(ts.depth, makeFunctionSymbol(x)(ts.copy(depth = ts.depth + 1)))))
    case x@LetDecl(name, _) => TransformerState(let = Map(name.dn -> Symbol(ts.depth, x)))
    case FunctionParam(_) => ???
    case Import(_) => ???
    case TokenTypes.Ignore => TransformerState()
  }

  def extractTypeDecl(vd: TypelevelDeclaration)(ts: TransformerState): TransformerState = vd match {
    case td@TypeDeclaration(_, _, expr) =>
      expr match {
        case DisjointUnion(_) => ??? //types.toList.map(tt => (tt.name.dn, (td, tt)))
      }
  }

  def symbolizeDeclaration(decl: Declaration)(ts: TransformerState): TransformerState = decl match {
    case decl: ValueDeclaration => extractValueDeclName(decl)(ts)
    case vd: TypelevelDeclaration => extractTypeDecl(vd)(ts)
    case TokenTypes.Ignore => TransformerState()
  }

  def symbolizeDeclarations(body: List[Declaration])(ts: TransformerState): TransformerState = {
    body.foldLeft(ts) { case (accum, next) =>
      accum combine symbolizeDeclaration(next)(accum)
    }
  }

  def transformFun(b: List[Declaration], e: Expression)
                  (ts: TransformerState): LCExp = {
    val nt = symbolizeDeclarations(b)(ts)
    transformExpr(e)(nt)
  }

  def application(v: ValueDeclaration, params: List[LCExp])
                 (t: TransformerState): LCExp = {
    val applyParams = (innerExpression: LCExp) => params.foldLeft(innerExpression) { case (accum, next) =>
      LCApplication(next, accum)
    }
    v match {
      case FunDecl(_, params, body) => {
        val stP = params.map { a =>
          val id = a.id.dn
          id -> Symbol(t.depth, LCName(id.name): LCExp)
        }.toMap
        val modT = t combine TransformerState(exp = stP)
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
    /*
    // Either its an already defined function, so we bind the parameters effectively unpacking it
    val eo = et.get(f.dn)
    // Or it is a symbol, eg a reference
    val so = st.get(f.dn).map(x => application(x, e.map(x => transformExpr(x)(t)))(t))
    // Or it is a type constructor
    val to = td.get(f.dn).map{ case (td, tt) => applyTypeConstructor(td, tt) }
    Seq(eo, so, to).flatten.headOption match {
      case Some(x) => x
      case None => throw new Exception(s"failed to find symbol ${f.dn.name}")
    }*/
  }

  def transformFunBody(b: Either[FunctionBody, Expression])
                      (t: TransformerState): LCExp = {
    val modT = t.copy(depth = t.depth + 1)
    b match {
      case Left(value) => transformFun(value.children, value.`end`)(modT)
      case Right(value) => transformExpr(value)(modT)
    }
  }

  def transformEntry(toplevelBody: List[Declaration]): LCExp = {
    val modT = declListToTS(toplevelBody)
    val (_, main): (Depth, FunDecl) = modT.func(DeclarationName("main"))
    transformFunBody(main.body)(modT.copy(depth = modT.depth + 1))
  }
}
