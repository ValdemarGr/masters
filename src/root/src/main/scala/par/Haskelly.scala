package par

import scala.language.postfixOps
import com.codecommit.gll._
import cats.data.NonEmptyList

object Haskelly extends Parsers with RegexParsers {
  object ADTS {
    sealed trait Op
    case object Addition extends Op

    sealed trait Expression
    final case class BinOp(l: Expression, op: Op, r: Expression) extends Expression
    final case class Var(name: String) extends Expression
    final case class Constructor(name: String) extends Expression
    final case class App(l: Expression, r: Expression) extends Expression
    final case class Let(name: String, e: Expression, next: Expression) extends Expression

    sealed trait Match
    final case class TypeConstructorMatch(constructorName: String, bindings: List[NameBindingMatch]) extends Match
    final case class NameBindingMatch(bindingName: String) extends Match

    final case class FunctionMeta(name: String, tpe: Option[TypeType])
    final case class FunctionImpl(ms: List[Match], expr: Expression)
    final case class FunctionGroup(meta: FunctionMeta, impls: NonEmptyList[FunctionImpl])

    sealed trait TypeType
    final case class ArrowType(from: TypeType, to: TypeType) extends TypeType
    final case class ProductType(name: String, typevars: List[TypeType]) extends TypeType
    final case class PolyType(name: String) extends TypeType

    final case class SumType(typename: String, typevars: List[String], products: NonEmptyList[ProductType])
  }
  import ADTS._

  val infixOp: Parser[Op] = (
    "+" ^^^ Addition
      //| "-" ^^^ Subtraction
      //| ">" ^^^ Geq
      //| "<" ^^^ Leq
      //| "==" ^^^ Equallity
      //| "!=" ^^^ Inequallity
  )

  val identifier: Parser[String] = """[a-z][a-zA-Z]*""".r
  val typeConstructorIdentifier: Parser[String] = """[A-Z][a-zA-Z]*""".r

  val constructor: Parser[Constructor] = typeConstructorIdentifier.map(Constructor)
  def parens: Parser[Expression] = "(" ~> expr <~ ")"
  val variable: Parser[Var] = identifier.map(Var)
  val let: Parser[Let] = ("let" ~> identifier) ~ ("=" ~> expr) ~ ("in" ~> expr) ^^ { (id, e, next) => Let(id, e, next) }
  //val app: Parser[App] = expr ~ expr ^^ { (l, r) => App(l, r) }
  val app: Parser[Expression] = "(" ~> (expr ~ (expr +)) <~ ")" ^^ { (l, rs) => rs.tail.fold(App(l, rs.head)){ case (accum, next) => App(accum, next) } }
  val binOp: Parser[BinOp] = expr ~ infixOp ~ expr ^^ { (l, o, r) => BinOp(l, o, r) }
  def expr: Parser[Expression] = parens | let | variable | constructor | binOp | app

  val tcMatch: Parser[TypeConstructorMatch] = typeConstructorIdentifier ~ (bindMatch *) ^^ { (n, s) => TypeConstructorMatch(n, s) }
  val bindMatch: Parser[NameBindingMatch] = identifier.map(NameBindingMatch)
  val parensMatch: Parser[Match] = "(" ~> matchE <~ ")"
  def matchE: Parser[Match] = parensMatch | tcMatch | bindMatch

  def funImpl: Parser[FunctionImpl] = (matchE *) ~ ("=" ~> expr) <~ ";" ^^ { (m, e) => FunctionImpl(m, e) }
  val funMeta: Parser[FunctionMeta] = identifier ~ (("::" ~> typetype) ?) <~ ";" ^^ { (fi, at) => FunctionMeta(fi, at) }
  val fun: Parser[FunctionGroup] = (funMeta ~ (funImpl +)) <~ ";" ^^ { (fm, fi) => FunctionGroup(fm, NonEmptyList(fi.head, fi.tail)) }

  def typetype: Parser[TypeType] = parensType | polytype | productType | arrowtype
  def arrowtype: Parser[TypeType] = "(" ~> (typetype ~ (("->" ~> typetype) +)) <~ ")" ^^ { (l, rs) => rs.tail.fold(ArrowType(l, rs.head)){ case (accum, next) => ArrowType(accum, next) } }
  val polytype: Parser[PolyType] = identifier.map(PolyType)
  val productType: Parser[ProductType] = typeConstructorIdentifier ~ (typetype *) ^^ { (ti, tt) => ProductType(ti, tt) }
  val sumType: Parser[SumType] = ("type" ~> typeConstructorIdentifier ~ ((identifier *) <~ "=") ~ (("|" ~> productType) +)) <~ ";" ^^ { (ti, vs, pts) => SumType(ti, vs, NonEmptyList(pts.head, pts.tail)) }
  val parensType: Parser[TypeType] = "(" ~> typetype <~ ")"

  val toplevel = (fun.map(Right(_)) | sumType.map(Left(_))) +

  def parse(s: String): List[Result[List[Either[SumType, FunctionGroup]]]] = 
    toplevel.apply(s).toList
}
