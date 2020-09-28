package ir

import cats.Monoid
import cats.data.NonEmptyList
import ir.LCLanguage._
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
                          applicable: List[Closure]
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

  def orderlessSymbolValue(vd: ValueDeclaration)(ss: SymbolState): SymbolState = vd match {
    case fd: FunDecl => SymbolState(func=Map(fd.varname.dn -> Symbol[FunDecl](ss.depth, fd)))
    case _: LetDecl => SymbolState()
    case Ignore => SymbolState()
    case _: Import => SymbolState()
  }

  def orderlessSymbol(decl: Declaration)(ss: SymbolState): SymbolState = decl match {
    case vd: ValueDeclaration => orderlessSymbolValue(vd)(ss)
    case _: TypelevelDeclaration => ???
    case Ignore => SymbolState()
  }

  def orderlessSymbols(decls: List[Declaration])(ss: SymbolState): SymbolState = decls.foldLeft(SymbolState()){ case (accum, next) =>
    accum combine orderlessSymbol(next)(ss)
  }

  def unwrapVD[A](f: FunDecl => A)(vd: ValueDeclaration)(ss: SymbolState)(implicit M: Monoid[A]): A = vd match {
    case fd: FunDecl => f(fd)
    case _: LetDecl => M.empty
    case Ignore => M.empty
    case _: Import => M.empty
  }

  def unwrapDecl[A](f: FunDecl => A)(decl: Declaration)(ss: SymbolState)(implicit M: Monoid[A]): A = decl match {
    case vd: ValueDeclaration => unwrapVD[A](f)(vd)(ss)
    case _: TypelevelDeclaration => ???
    case Ignore => M.empty
  }

  def unwrapDecls[A](decls: List[Declaration])(ss: SymbolState)(f: FunDecl => A)(implicit M: Monoid[A]): A = decls.foldLeft(M.empty){ case (accum, next) =>
    M.combine(accum, unwrapDecl[A](f)(next)(ss))
  }

  def genPrimeValue(vd: ValueDeclaration)(ss: SymbolState): (List[LCBinding], SymbolState) = vd match {
    case fd: FunDecl => Nil -> SymbolState(func=Map(fd.varname.dn -> Symbol[FunDecl](ss.depth, fd)))
    case _: LetDecl => Nil -> SymbolState()
    case Ignore => Nil -> SymbolState()
    case _: Import => Nil -> SymbolState()
  }

  def genPrimeFunc(decl: Declaration)(ss: SymbolState): (List[LCBinding], SymbolState) =  decl match {
    case vd: ValueDeclaration => genPrimeValue(vd)(ss)
    case _: TypelevelDeclaration => ???
    case Ignore => Nil -> SymbolState()
  }

  def genPrimeFuncs(decls: List[Declaration])(ss: SymbolState): (List[LCBinding], SymbolState) = decls
    .foldLeft(List.empty[LCBinding] -> SymbolState()){ case ((accumLCs, ss), next) =>
      val (bindings, s) = genPrimeFunc(next)(ss)
      (accumLCs ++ bindings, ss combine s)
    }
}
