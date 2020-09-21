package ir

import cats.data.NonEmptyList
import par.TokenTypes
import par.TokenTypes._
import LCLanguage._

object LCTransform {
  case class DeclarationName(name: String)

  type K = DeclarationName
  type V = ValueDeclaration
  type Symbol = (K, V)
  type SymbolTable = Map[K, V]

  type LCExpTable = Map[K, LCExp]

  implicit class NELDN(nel: NonEmptyList[Char]) {
    val dn = DeclarationName(nel.toList.mkString)
  }
  implicit class SafeGet(st: SymbolTable) {
    def safeGet(k: K): V = st.get(k) match {
      case Some(v) => v
      case None => throw new Exception(s"failed to get key ${k} in symbol table ${st}")
    }
  }

  def extractValueDeclName(vd: ValueDeclaration): Option[Symbol] = vd match {
    case x@FunDecl(name, _, _) => Some(name.dn -> x)
    case x@Import(imp) => Some(imp.dn -> x)
    case x@LetDecl(name, _) => Some(name.dn -> x)
    case x@FunctionParam(name) => Some(name.dn -> x)
    case TokenTypes.Ignore => None
  }

  def extractDeclName(decl: Declaration): Option[Symbol] = decl match {
    case decl: ValueDeclaration => extractValueDeclName(decl)
    case _: TypelevelDeclaration => None
    case TokenTypes.Ignore => None
  }

  def declListToSymbolTable(body: List[Declaration]): SymbolTable =
    body.foldLeft(Map.empty[K, V]) { case (accum, next) =>
      accum ++ extractDeclName(next).toMap
    }

  def transformFun(b: List[Declaration], e: Expression)(st: SymbolTable, et: LCExpTable): LCExp = {
    val fSt = st ++ declListToSymbolTable(b)
    transformExpr(e)(fSt, et)
  }

  def transformValueDecl(v: ValueDeclaration, binds: List[LCExp])(st: SymbolTable, et: LCExpTable): LCExp = v match {
    case FunDecl(_, params, body) => {
      val stP = params.zip(binds).map{ case (p, b) =>
        p.id.dn -> b
      }.toMap
      val modEt = et ++ stP
      transformFunBody(body)(st, modEt)
    }
    case LetDecl(_, value) => transformExpr(value)(st, et)
    case FunctionParam(_) => ???
    case Import(_) => ???
    case TokenTypes.Ignore => ???
  }

  def transformExpr(e: Expression)(st: SymbolTable, et: LCExpTable): LCExp = e match {
    case InfixBuiltin(lhs, op, rhs) => {
      val fst = LCName("fst")
      val snd = LCName("snd")
      val f = LCFunction("infix1", fst, LCFunction("infix2", snd, LCTerminalOperation(fst, op, snd)))
      val lhEval = transformExpr(lhs)(st,et )
      val rhEval = transformExpr(rhs)(st,et )
      LCApplication(LCApplication(f, lhEval), rhEval)
    }
    case FunctionBody(children, end) => transformFun(children, end)(st, et)
    case Apply(f, e) =>
      et.getOrElse(f.dn, transformValueDecl(st.safeGet(f.dn), e map (x => transformExpr(x)(st, et)))(st, et))
    case ConstantInteger(n) => LCNumber(n.dn.name.toInt)
    case ConstantStr(s) => LCString(s.mkString)
  }

  def transformFunBody(b: Either[FunctionBody, Expression])(st: SymbolTable, et: LCExpTable): LCExp = b match {
    case Left(value) => transformFun(value.children, value.`end`)(st, et)
    case Right(value) => transformExpr(value)(st, et)
  }

  def transformEntry(toplevelBody: List[Declaration]): LCExp = {
    val symbolTable: SymbolTable = declListToSymbolTable(toplevelBody)
    val main: FunDecl = symbolTable.safeGet(DeclarationName("main")) match {
      case x: FunDecl => x
      case Import(_) => throw new Exception("main was an import")
      case LetDecl(_, _) => throw new Exception("main was a let declaration")
      case x => throw new Exception(s"main was ${x}")
    }
    transformFunBody(main.body)(symbolTable, Map.empty)
  }
}
