package ir

import cats.data.NonEmptyList
import par.TokenTypes
import par.TokenTypes._
import LCLanguage._

object LCTransform {
  case class DeclarationName(name: String)

  type K = DeclarationName
  type V = Either[ValueDeclaration, LCExp]
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
    case x@FunDecl(name, _, _) => Some(name.dn -> Left(x))
    case x@Import(imp) => Some(imp.dn -> Left(x))
    case x@LetDecl(name, _) => Some(name.dn -> Left(x))
    case x@FunctionParam(name) => Some(name.dn -> Left(x))
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

  def transformValueDecl(v: ValueDeclaration, binds: List[LCExp])(st: SymbolTable): LCExp = v match {
    case FunDecl(_, params, body) => {
      val stP = params.zip(binds).map{ case (p, b) =>
        p.id.dn -> Right(b)
      }.toMap
      transformFunBody(body)(st ++ stP)
      /*params.reverse match {
        case Nil => b
        case x :: xs => {
          val inner = LCFunction(fname.dn.name, LCName(x.id.dn.name), b)
          xs.zipWithIndex.foldLeft(inner){ case (accum, (next, i)) =>
            LCFunction(fname.dn.name + s"-param-${i}", LCName(next.id.dn.name), accum)
          }
        }
      }*/
    }
    case LetDecl(_, value) => transformExpr(value)(st)
    case FunctionParam(_) => ???
    case Import(_) => ???
    case TokenTypes.Ignore => ???
  }

  def transformExpr(e: Expression)(st: SymbolTable): LCExp = e match {
    case InfixBuiltin(lhs, op, rhs) => {
      val fst = LCName("fst")
      val snd = LCName("snd")
      val f = LCFunction("infix1", fst, LCFunction("infix2", snd, LCTerminalOperation(fst, op, snd)))
      val lhEval = transformExpr(lhs)(st)
      val rhEval = transformExpr(rhs)(st)
      LCApplication(LCApplication(f, lhEval), rhEval)
    }
    case FunctionBody(children, end) => transformFun(children, end)(st)
    case Apply(f, e) => {
      st.safeGet(f.dn) match {
        case Right(v) => v
        case Left(fName) =>
          transformValueDecl(fName, e map (x => transformExpr(x)(st)))(st)
      }
    }
    case ConstantInteger(n) => LCNumber(n.dn.name.toInt)
    case ConstantStr(s) => LCString(s.mkString)
  }

  def transformFunBody(b: Either[FunctionBody, Expression])(st: SymbolTable) = b match {
    case Left(value) => transformFun(value.children, value.`end`)(st)
    case Right(value) => transformExpr(value)(st)
  }

  def transformEntry(toplevelBody: List[Declaration]): LCExp = {
    val symbolTable: SymbolTable = declListToSymbolTable(toplevelBody)
    val main: FunDecl = symbolTable.safeGet(DeclarationName("main")) match {
      case Left(x: FunDecl) => x
      case Left(Import(_)) => throw new Exception("main was an import")
      case Left(LetDecl(_, _)) => throw new Exception("main was a let declaration")
      case x => throw new Exception(s"main was ${x}")
    }
    transformFunBody(main.body)(symbolTable)
  }
}
