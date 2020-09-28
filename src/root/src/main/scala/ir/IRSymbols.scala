package ir

import cats.Monoid
import cats.data.NonEmptyList
import ir.LCLanguage.LCExp
import par.TokenTypes._

object IRSymbols {
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

  case class UnappliedFunc(
                          source: FunDecl,
                          closures: List[Closure]
                          )
  type UnappliedFuncTable = Map[String, Symbol[UnappliedFunc]]

  case class ParameterSymbol(
                              symbolName: String
                            )

  type ParameterTable = Map[String, Symbol[ParameterSymbol]]

  implicit val tsMonoid = new Monoid[SymbolState] {
    override def empty: SymbolState = SymbolState()

    override def combine(x: SymbolState, y: SymbolState): SymbolState =
      SymbolState(
        x.let ++ y.let,
        x.typeDecl ++ y.typeDecl,
        x.func ++ y.func,
        x.unappliedFunc ++ y.unappliedFunc,
        x.param ++ y.param,
        depth=Math.max(x.depth, y.depth)
      )
  }

  implicit class TSOps(ts: SymbolState)(implicit M: Monoid[SymbolState]) {
    def combine(that: SymbolState): SymbolState = M.combine(ts, that)
  }

  case class SymbolState(
                               let: LetTable = Map.empty,
                               typeDecl: TypeDeclTable = Map.empty,
                               func: FunctionTable = Map.empty,
                               unappliedFunc: UnappliedFuncTable = Map.empty,
                               param: ParameterTable = Map.empty,
                               depth: Depth = 0,
                             )
  implicit class NELTXT(nel: NonEmptyList[Char]) {
    val dn = DeclarationName(nel.toList.mkString)
    val str = dn.name
  }

  def orderlessSymbolFromValueDecl(vd: ValueDeclaration)(ss: SymbolState): SymbolState = vd match {
    case fd: FunDecl => SymbolState(func=Map(fd.varname.dn -> Symbol[FunDecl](ss.depth, fd)))
    case _: LetDecl => SymbolState()
    case Ignore => SymbolState()
    case _: Import => SymbolState()
  }

  def orderlessSymbolFromDecl(decl: Declaration)(ss: SymbolState): SymbolState = decl match {
    case vd: ValueDeclaration => orderlessSymbolFromValueDecl(vd)(ss)
    case _: TypelevelDeclaration => ???
    case Ignore => SymbolState()
  }

  def orderlessSymbols(decls: List[Declaration])(ss: SymbolState): SymbolState = decls.foldLeft(SymbolState()){ case (accum, next) =>
    accum combine orderlessSymbolFromDecl(next)(ss)
  }
}
