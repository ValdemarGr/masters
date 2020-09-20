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

  def spaces[A](p: Parser[A]): Parser[A] = many(spaceChar) ~> p <~ many(spaceChar)
  val w = skipMany1(whitespace)
  val space = skipMany1(whitespace | char('\n'))
  val endDecl = many(spaceChar) ~> (char('\n') | char(';'))
  val let = string("let") ~> many(spaceChar)
  val fun = string("fun") ~> many(spaceChar)
  val id = spaces(many1(letter))
  val infixBuiltin: Parser[BuiltinOperator] = {
    val addition = char('+') >| Addition
    addition
  }
  val `=` = spaces(char('='))
  val `;` = many1(char(';'))
  val `type` = string("type") ~> many(spaceChar)
  val idParams = id ~ many(id)

  def excludeText(exclude: Char) = many(elem(_ != exclude))

  def t3[A, B, C](t: ((A, B), C)): (A, B, C) = t match { case ((a, b), c) => (a, b, c) }

  val number: Parser[Expression] = many1(digit) map ConstantInteger
  val str: Parser[Expression] = bracket(char('\''), excludeText('\''), char('\'')) map ConstantStr
  val app: Parser[Expression] = id ~ many(expression) map Apply.tupled
  val infixOp: Parser[Expression] = app ~ infixBuiltin ~ app map t3 map InfixBuiltin.tupled
  val expBody: Parser[Expression] = spaces(number | str | infixOp | app)
  val expression = parens(expBody) | expBody

  val imp: Parser[ValueDeclaration] = (string("import") ~> w ~> id) <~ endDecl map Import
  val letDecl: Parser[ValueDeclaration] = (let ~> id <~ `=`) ~ expression <~ endDecl map LetDecl.tupled
  val funDecl: Parser[ValueDeclaration] =
    (fun ~> (idParams) <~ `=`) ~
    (functionBody || (expression <~ endDecl)) map t3 map { case (x1, x2, x3) => FunDecl(x1, x2 map FunctionParam, x3)}
  val declaration: Parser[ValueDeclaration] = letDecl | funDecl | imp
  val comment = string("//") ~> excludeText('\n') >| Ignore
  val redundantNl = many1(char('\n'))  >| Ignore
  val emptySpace = skipMany1(whitespace) >| Ignore

  val typeName: Parser[TypeParam] = id map TypeName
  val parensType: Parser[TypeParam] = parens(typeParams) map ParensType
  val typeParams: Parser[List[TypeParam]] = many(typeName | parensType)
  val tagType = id ~ typeParams <~ many(spaceChar) map TagType.tupled
  val disjointUnion =
    tagType ~ many(char('|') ~> tagType) map { case (x, xs) => DisjointUnion(NonEmptyList(x, xs)) }
  val typeDecl: Parser[TypeDeclaration] =
    (`type` ~> idParams <~ `=`) ~ disjointUnion map t3 map TypeDeclaration.tupled
  val typelevelDecl = typeDecl <~ endDecl

  val functionBody = (many(
        declaration
      | emptySpace
      | redundantNl
      | comment
  ) ~ (expression <~ endDecl)) map FunctionBody.tupled

  val body: Parser[List[Declaration]] = many(declaration | typelevelDecl | emptySpace | redundantNl | comment)

  val parser: Parser[List[Declaration]] = token(body)
}
