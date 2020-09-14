package tokens

import cats.data.NonEmptyList

object TokenTypes {
  trait Token

  trait Declaration extends Token
  case class Import(imp: NonEmptyList[Char]) extends Declaration
  case class LetDecl(varname: NonEmptyList[Char], value: Expression) extends Declaration
  case class FunDecl(varname: NonEmptyList[Char], params: List[NonEmptyList[Char]], body: Either[Expression, FunctionBody]) extends Declaration

  trait Expression extends Token
  case class InfixBuiltin(op: Char) extends Expression
  case class Reference(name: NonEmptyList[Char]) extends Expression
  trait Constant extends Expression
  case class ConstantInteger(v: NonEmptyList[Char]) extends Constant
  case class ConstantStr(v: List[Char]) extends Constant

  case class Invocation(vs: NonEmptyList[Expression]) extends Expression

  case class FunctionBody(children: List[Declaration], end: Expression) extends Expression

  case object Ignore extends Expression with Declaration
}
