package tokens

import atto._
import Atto._

import scala.util.matching.Regex

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

  def regex(r: Regex): Parser[Char] = elem(x => r.findFirstIn(x.toString).isDefined)

  val w = skipMany1(whitespace)
  val space = skipMany1(whitespace | char('\n'))
  val endDecl = many(spaceChar) ~> (char('\n') | char(';'))
  val let = string("let") ~> space
  val fun = string("fun") ~> space
  val id = many1(letter)
  val infixBuiltin = oneOf("+-*/")
  val `=` = w ~ char('=') ~ w
  val `;` = many1(char(';'))
  val textR = "[a-zA-Z0-9 ,.=';]".r
  val text = many(regex(textR))

  val number: Parser[Expression] = many1(digit) map ConstantInteger
  val str: Parser[Expression] = bracket(char('\''), text, char('\'')) map ConstantStr
  val expression: Parser[Expression] = number | str

  val imp: Parser[Declaration] = (string("import") ~> w ~> id) <~ endDecl map Import
  val letDecl: Parser[Declaration] = (let ~> id <~ `=`) ~ expression <~ endDecl map LetDecl.tupled
  val funDecl: Parser[Declaration] =
    (fun ~> (id ~ many(w ~> id)) <~ `=`) ~
    ((expression <~ endDecl) || functionBody) map { case ((a, b), c) => FunDecl(a,b,c) }
  val declaration: Parser[Declaration] = letDecl | funDecl | imp
  val comment = string("//") ~> text >| Ignore
  val redundantNl = many1(char('\n'))  >| Ignore
  val emptySpace = skipMany1(whitespace) >| Ignore

  val functionBody = (many(
        declaration
      | emptySpace
      | redundantNl
      | comment
  ) ~ (expression <~ endDecl)) map FunctionBody.tupled
  val body = many(declaration | emptySpace | redundantNl | comment)

  val parser: Parser[List[Declaration]] = token(body)
}
