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

  def transformFun(b: List[Declaration], e: Expression)(st: SymbolTable): LCExp = {
    val fSt = st ++ declListToSymbolTable(b)
    transformExpr(e)(fSt)
  }

  def transformValueDecl(v: ValueDeclaration)(st: SymbolTable) = v match {
    case FunDecl(_, params, body) => {
      val b = transformFunBody(body)(st)
      params.reverse match {
        case Nil => b
        case x :: xs => {
          val inner = LCFunction(LCName(x.dn.name), b)
          xs.foldLeft(inner){ case (accum, next) =>
            LCFunction(LCName(next.dn.name), accum)
          }
        }
      }
    }
    case LetDecl(_, value) => transformExpr(value)(st)
    case Import(_) => ???
    case TokenTypes.Ignore => ???
  }

  def transformDecl(d: Declaration)(st: SymbolTable) = d match {
    case decl: ValueDeclaration => transformValueDecl(decl)(st)
    case _: TypelevelDeclaration => ???
    case TokenTypes.Ignore => ???
  }

  def transformExpr(e: Expression)(st: SymbolTable): LCExp = e match {
    case InfixBuiltin(lhs, op, rhs) => {
      val fst = LCName("fst")
      val snd = LCName("snd")
      val f = LCFunction(fst, LCFunction(snd, LCTerminalOperation(fst, op, snd)))
      val lhEval = transformExpr(lhs)(st)
      val rhEval = transformExpr(rhs)(st)
      LCApplication(LCApplication(f, lhEval), rhEval)
    }
    case FunctionBody(children, end) => transformFun(children, end)(st)
    case Apply(f, e) => {
      val fName = st.safeGet(f.dn)
      val asD = transformValueDecl(fName)(st)
      LCDebug(s"apply ${fName} to ${e.map(x => transformExpr(x)(st))}\n${asD}")
    }
    case ConstantInteger(n) => LCDebug(s"number ${n}")
    case ConstantStr(s) => LCDebug(s"str ${s}")
  }

  def transformFunBody(b: Either[FunctionBody, Expression])(st: SymbolTable) = b match {
    case Left(value) => transformFun(value.children, value.`end`)(st)
    case Right(value) => transformExpr(value)(st)
  }

  def transformEntry(toplevelBody: List[Declaration]): LCExp = {
    val symbolTable: SymbolTable = declListToSymbolTable(toplevelBody)
    val main: FunDecl = symbolTable.safeGet(DeclarationName("main")) match {
      case x: FunDecl => x
      case Import(_) => throw new Exception("main was an import")
      case LetDecl(_, _) => throw new Exception("main was a let declaration")
    }
    transformFunBody(main.body)(symbolTable)
  }
}
