package tokens

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

  val w = skipMany1(whitespace)
  val space = skipMany1(whitespace | char('\n'))
  val endDecl = many(spaceChar) ~> (char('\n') | char(';'))
  val let = string("let") ~> many(spaceChar)
  val fun = string("fun") ~> many(spaceChar)
  val id = many(spaceChar) ~> many1(letter) <~ many(spaceChar)
  val infixBuiltin = oneOf("+-*/")
  val `=` = many(spaceChar) ~> char('=') <~ many(spaceChar)
  val `;` = many1(char(';'))
  val `type` = string("type") ~> many(spaceChar)
  val idParams = id ~ many(id)

  def excludeText(exclude: Char) = many(anyChar.filter(_ != exclude))

  def t3[A, B, C](t: ((A, B), C)): (A, B, C) = t match { case ((a, b), c) => (a, b, c) }

  val number: Parser[Expression] = many1(digit) map ConstantInteger
  val str: Parser[Expression] = bracket(char('\''), excludeText('\''), char('\'')) map ConstantStr
  val infixOp: Parser[Expression] = id ~ infixBuiltin ~ id map t3 map InfixBuiltin.tupled
  val expression: Parser[Expression] = number | str | infixOp

  val imp: Parser[ValueDeclaration] = (string("import") ~> w ~> id) <~ endDecl map Import
  val letDecl: Parser[ValueDeclaration] = (let ~> id <~ `=`) ~ expression <~ endDecl map LetDecl.tupled
  val funDecl: Parser[ValueDeclaration] =
    (fun ~> idParams <~ `=`) ~
    ((expression <~ endDecl) || functionBody) map t3 map FunDecl.tupled
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
