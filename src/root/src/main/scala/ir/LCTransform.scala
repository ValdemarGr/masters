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

  type TypeV = (TypeDeclaration, TagType)
  type TypeSymbol = (K, TypeV)
  type TypeDeclTable = Map[K, TypeV]

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

  def extractTypeDecl(vd: TypelevelDeclaration): List[TypeSymbol] = vd match {
    case td@TypeDeclaration(_, _, expr) =>
      expr match {
        case DisjointUnion(types) => types.toList.map(tt => (tt.name.dn, (td, tt)))
      }
  }

  def extractDeclName(decl: Declaration): List[Either[Symbol, TypeSymbol]] = decl match {
    case decl: ValueDeclaration => extractValueDeclName(decl).toList.map(Left(_))
    case vd: TypelevelDeclaration => extractTypeDecl(vd).map(Right(_))
    case TokenTypes.Ignore => List.empty
  }

  def declListToSymbolTable(body: List[Declaration]): (SymbolTable, TypeDeclTable) = {
    val s = body.foldLeft(Seq.empty[Either[Symbol, TypeSymbol]]) { case (accum, next) =>
      accum ++ extractDeclName(next).toSeq
    }
    (
      s.collect{ case Left(x) => x }.toMap,
      s.collect{ case Right(x) => x }.toMap
    )
  }

  def transformFun(b: List[Declaration], e: Expression)
                  (st: SymbolTable, et: LCExpTable, td: TypeDeclTable): LCExp = {
    val (s, t) = declListToSymbolTable(b)
    val fSt = st ++ s
    transformExpr(e)(fSt, et, td ++ t)
  }

  def transformValueDecl(v: ValueDeclaration, binds: List[LCExp])
                        (st: SymbolTable, et: LCExpTable, td: TypeDeclTable): LCExp = v match {
    case FunDecl(_, params, body) => {
      val stP = params.zip(binds).map{ case (p, b) =>
        p.id.dn -> b
      }.toMap
      val modEt = et ++ stP
      transformFunBody(body)(st, modEt, td)
    }
    case LetDecl(_, value) => transformExpr(value)(st, et, td)
    case FunctionParam(_) => ???
    case Import(_) => ???
    case TokenTypes.Ignore => ???
  }

  def applyTypeConstructor(t: TypeDeclaration, tt: TagType): LCExp =
    t.expr match {
      case DisjointUnion(types) =>
        val pos = types
          .zipWithIndex
          .find{ case (x, _) => x == tt}
          .map{ case (_, i) => i }
          .getOrElse(throw new Exception(s"failed to find ${tt} in ${t}"))
        println(pos)
        ???
    }

  def transformExpr(e: Expression)
                   (st: SymbolTable, et: LCExpTable, td: TypeDeclTable): LCExp = e match {
    case InfixBuiltin(lhs, op, rhs) => {
      val fst = LCName("fst")
      val snd = LCName("snd")
      val f = LCFunction("infix1", fst, LCFunction("infix2", snd, LCTerminalOperation(fst, op, snd)))
      val lhEval = transformExpr(lhs)(st, et, td)
      val rhEval = transformExpr(rhs)(st, et, td)
      LCApplication(LCApplication(f, lhEval), rhEval)
    }
    case FunctionBody(children, end) => transformFun(children, end)(st, et, td)
    case ConstantInteger(n) => LCNumber(n.dn.name.toInt)
    case ConstantStr(s) => LCString(s.mkString)
    case Apply(f, e) =>
      // Either its an already defined function, so we bind the parameters effectively unpacking it
      val eo = et.get(f.dn)
      // Or it is a symbol, eg a reference
      val so = st.get(f.dn).map(x => transformValueDecl(x, e.map(x => transformExpr(x)(st, et, td)))(st, et, td))
      // Or it is a type constructor
      val to = td.get(f.dn).map{ case (td, tt) => applyTypeConstructor(td, tt) }
      Seq(eo, so, to).flatten.headOption match {
        case Some(x) => x
        case None => throw new Exception(s"failed to find symbol ${f.dn.name}")
      }
  }

  def transformFunBody(b: Either[FunctionBody, Expression])
                      (st: SymbolTable, et: LCExpTable, td: TypeDeclTable): LCExp = b match {
    case Left(value) => transformFun(value.children, value.`end`)(st, et, td)
    case Right(value) => transformExpr(value)(st, et, td)
  }

  def transformEntry(toplevelBody: List[Declaration]): LCExp = {
    val (symbolTable, td) = declListToSymbolTable(toplevelBody)
    val main: FunDecl = symbolTable.safeGet(DeclarationName("main")) match {
      case x: FunDecl => x
      case Import(_) => throw new Exception("main was an import")
      case LetDecl(_, _) => throw new Exception("main was a let declaration")
      case x => throw new Exception(s"main was ${x}")
    }
    transformFunBody(main.body)(symbolTable, Map.empty, td)
  }
}
