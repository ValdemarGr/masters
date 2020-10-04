package par

import atto._
import Atto._
import cats.data.NonEmptyList

object TokenCombinators {
  import TokenTypes._
  implicit class ParserTap[+A](p: Parser[A]) {
    def tap(f: A => Unit): Parser[A] = p.map { value =>
      f(value)
      value
    }

    def ptap(name: String): Parser[A] = tap{ value =>
      println(s"in ${name} got value ${value}")
    }
  }

  def spaces[A](p: => Parser[A]): Parser[A] = many(spaceChar) ~> p <~ many(spaceChar)
  val endDecl = spaces(char('\n').ptap("end nl") | char(';').ptap("end ;"))
  val let = string("let") ~> many(spaceChar)
  val fun = string("fun") ~> many(spaceChar)
  val id = spaces(many1(letter | char('_')))
  val infixBuiltin: Parser[BuiltinOperator] = {
    val addition = char('+') >| Addition
    val subtraction = char('-') >| Subtraction
    addition | subtraction
  }
  val `=` = spaces(char('='))
  val `type` = string("type") ~> many(spaceChar)
  val `if` = spaces(string("if"))
  val `else` = spaces(string("else"))
  val idParams = id ~ many(id)

  def excludeText(exclude: Char) = many(elem(_ != exclude))

  def t3[A, B, C](t: ((A, B), C)): (A, B, C) = t match { case ((a, b), c) => (a, b, c) }

  //val number: Parser[Expression] = many1(digit).ptap("number") map ConstantInteger
  //val str: Parser[Expression] = bracket(char('\''), excludeText('\''), char('\'')) map ConstantStr
  //val app: Parser[Expression] = id.ptap("app id") ~ many(expression).ptap("exprs") map Apply.tupled
  //val infixOp: Parser[Expression] = number.ptap("infix lhs") ~ infixBuiltin.ptap("infix op") ~ number.ptap("infix rhs") map t3 map InfixBuiltin.tupled
  //val expBody: Parser[Expression] = spaces(number | str.ptap("str") | infixOp | app)
  //val expression = spaces(parens(expBody).ptap("expr parens") | expBody.ptap("expr no parens"))

  //val number: Parser[Expression] = spaces(int.ptap("int")) map ConstantInteger
  //val app: Parser[Expression] = spaces(id.ptap("app id")) ~ spaces(many(expression.ptap("app expr"))) map Apply.tupled
  //val infixExpr: Parser[Expression] = spaces(number | parens(app) | parens(infix))
  //val infix: Parser[Expression] = spaces(infixExpr.ptap("infix lhs")) ~ spaces(infixBuiltin.ptap("infix")) ~ spaces(infixExpr.ptap("infix rhs")) map t3 map InfixBuiltin.tupled
  //val expressionType: Parser[Expression] = spaces(infix | number | app).ptap("expr type")
  //val expression: Parser[Expression] = spaces(parens(expressionType).ptap("parens") | expressionType.ptap("no parens"))

  val number: Parser[Expression] = spaces(int.ptap("int")) map ConstantInteger
  val app: Parser[Expression] =
    spaces(id.ptap("app id")) ~ spaces(many(expression.ptap("app expr"))) map Apply.tupled
  val infix: Parser[Expression] =
    parens(spaces(expression.ptap("infix lhs")) ~ spaces(infixBuiltin.ptap("infix")) ~ spaces(expression.ptap("infix rhs")) map t3 map InfixBuiltin.tupled)
  val conditional: Parser[Expression] = `if`.ptap("if") ~> (parens(expression) ~ functionBody <~ `else`) ~ functionBody map t3 map If.tupled
  val expression: Parser[Expression] = spaces((conditional | number | infix | parens(app) | app).ptap("parens"))

  val imp: Parser[ValueDeclaration] = (string("import") ~> spaces(id)) <~ endDecl map Import
  val letDecl: Parser[ValueDeclaration] = (let ~> id <~ `=`) ~ expression <~ endDecl map LetDecl.tupled
  val funDecl: Parser[ValueDeclaration] =
    (fun ~> (idParams.ptap("func id params")) <~ `=`.ptap("eq")) ~
    functionBody map t3 map { case (x1, x2, x3) => FunDecl(x1, x2 map FunctionParam, x3)}
  val declaration: Parser[ValueDeclaration] = letDecl | funDecl | imp
  val comment = string("//") ~> excludeText('\n') >| Ignore
  val redundantNl = many1(char('\n'))  >| Ignore

  val typeName: Parser[TypeParam] = id map TypeName
  val parensType: Parser[TypeParam] = parens(tagType) map ParensType
  val typeParams: Parser[List[TypeParam]] = many(typeName | parensType)
  val tagType = id ~ typeParams <~ many(spaceChar) map TagType.tupled
  val disjointUnion =
    tagType ~ many(char('|') ~> tagType) map { case (x, xs) => DisjointUnion(NonEmptyList(x, xs)) }
  val typeDecl: Parser[TypeDeclaration] =
    (`type` ~> idParams <~ `=`) ~ disjointUnion map t3 map TypeDeclaration.tupled
  val typelevelDecl = typeDecl <~ endDecl

  val functionBody = (many(
        declaration.ptap("decl")
      | redundantNl.ptap("redundant nl")
      | (spaceChar >| Ignore).ptap("space")
      | comment.ptap("comment")
  ) ~ (expression.ptap("fun expr") <~ endDecl.ptap("end fun decl"))) map FunctionBody.tupled

  val body: Parser[List[Declaration]] = many(declaration | typelevelDecl | redundantNl | comment)

  val parser: Parser[List[Declaration]] = token(body)
}
