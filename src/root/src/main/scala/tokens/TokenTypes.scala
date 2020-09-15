package tokens

import cats.data.NonEmptyList

object TokenTypes {
  trait Token
  type Identifier = NonEmptyList[Char]

  trait Declaration extends Token
  trait ValueDeclaration extends Declaration
  case class Import(imp: Identifier) extends ValueDeclaration
  case class LetDecl(varname: Identifier, value: Expression) extends ValueDeclaration
  case class FunDecl(varname: Identifier, params: List[Identifier], body: Either[Expression, FunctionBody]) extends ValueDeclaration

  trait TypelevelDeclaration extends Declaration
  case class TypeDeclaration(typename: Identifier, typeParams: List[Identifier], expr: TypelevelExpression) extends TypelevelDeclaration

  trait TypelevelExpression extends Token
  case class DisjointUnion(types: NonEmptyList[TagType]) extends TypelevelExpression
  case class TagType(name: Identifier, ids: List[TypeParam])

  trait TypeParam
  case class ParensType(params: List[TypeParam]) extends TypeParam
  case class TypeName(name: Identifier) extends TypeParam

  trait Expression extends Token
  case class InfixBuiltin(lhs: Identifier, op: Char, rhs: Identifier) extends Expression
  case class Reference(name: Identifier) extends Expression
  case class FunctionBody(children: List[ValueDeclaration], end: Expression) extends Expression
  case class Invocation(vs: NonEmptyList[Expression]) extends Expression

  trait Constant extends Expression
  case class ConstantInteger(v: Identifier) extends Constant
  case class ConstantStr(v: List[Char]) extends Constant

  case object Ignore extends Expression with Declaration with ValueDeclaration
}
