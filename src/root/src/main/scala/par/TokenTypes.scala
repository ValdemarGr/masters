package par

import cats.data.NonEmptyList

object TokenTypes {
  trait Token
  type Identifier = String

  trait Declaration extends Token
  trait ValueDeclaration extends Declaration
  case class FunctionParam(id: Identifier)
  case class Import(imp: Identifier) extends ValueDeclaration
  case class LetDecl(varname: Identifier, value: Expression) extends ValueDeclaration
  case class FunDecl(varname: Identifier, params: List[FunctionParam], body: FunctionBody) extends ValueDeclaration
  case class FunctionBody(children: List[Declaration], end: Expression)

  trait TypelevelDeclaration extends Declaration
  case class TypeDeclaration(typename: Identifier, typeParams: List[Identifier], expr: DisjointUnion) extends TypelevelDeclaration

  trait TypelevelExpression extends Token
  case class DisjointUnion(types: NonEmptyList[TagType]) extends TypelevelExpression
  case class TagType(name: Identifier, ids: List[TypeParam])

  trait TypeParam
  case class ParensType(inner: TagType) extends TypeParam
  case class TypeName(name: Identifier) extends TypeParam

  trait BuiltinOperator
  case object Addition extends BuiltinOperator
  case object Subtraction extends BuiltinOperator
  case object Equallity extends BuiltinOperator
  case object Inequallity extends BuiltinOperator
  case object Geq extends BuiltinOperator
  case object Leq extends BuiltinOperator

  trait Expression extends Token
  case class InfixBuiltin(lhs: Expression, op: BuiltinOperator, rhs: Expression) extends Expression
  case class Apply(name: Identifier, vs: List[Expression]) extends Expression
  case class If(expr: Expression, fst: FunctionBody, snd: FunctionBody) extends Expression

  case class PatternMatch(expr: Expression, cases: NonEmptyList[MatchCase]) extends Expression
  case class MatchCase(typeConstructor: Identifier, bindings: List[Identifier], body: FunctionBody)

  trait Constant extends Expression
  case class ConstantInteger(v: Int) extends Constant
  case class ConstantStr(v: List[Char]) extends Constant

  case object Ignore extends Expression with Declaration with ValueDeclaration
}
