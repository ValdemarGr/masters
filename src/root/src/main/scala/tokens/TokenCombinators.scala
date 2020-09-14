package tokens

import atto._
import Atto._

import scala.util.matching.Regex

object TokenCombinators {
  import TokenTypes._

  def regex(r: Regex): Parser[Char] = elem(x => r.findFirstIn(x.toString).isDefined)

  val w = skipMany1(whitespace)
  val space = skipMany1(whitespace | char('\n'))
  val newlineEnd = many(spaceChar) ~> char('\n')
  val let = string("let") ~> space
  val fun = string("fun") ~> space
  val id = many1(letter)
  val infixBuiltin = oneOf("+-*/")
  val `=` = w <~ char('=') <~ w
  val `;` = many1(char(';'))
  val textR = "[a-zA-Z0-9 ,.]".r
  val text = many(regex(textR))

  val number: Parser[Expression] = many1(digit) map ConstantInteger
  val str: Parser[Expression] = bracket(char('\''), text, char('\'')) map ConstantStr
  val expression: Parser[Expression] = number | str

  val imp: Parser[Declaration] = (string("import") ~> w ~> id) map Import
  val letDecl: Parser[Declaration] = (let ~> id <~ `=`) ~ expression map LetDecl.tupled
  val funDecl: Parser[Declaration] = (fun ~> (id ~ many(w ~> id)) <~ `=`) ~ (expression || functionBody) map { case ((a, b), c) => FunDecl(a,b,c) }
  val declaration: Parser[Declaration] = (letDecl | funDecl | imp) <~ (`;` | newlineEnd)
  val comment = string("//") ~> text >| Ignore
  val redundantNl = many1(char('\n'))  >| Ignore
  val emptySpace = skipMany1(whitespace) >| Ignore

  val functionBody = (many(declaration | emptySpace | redundantNl | comment) ~ (expression <~ newlineEnd)) map FunctionBody.tupled
  val body = many(declaration | emptySpace | redundantNl | comment)

  val parser: Parser[List[Declaration]] = token(body)

/*
  val str: Parser[Constant] = bracket (char('\''), many1(digit | anyChar), char('\'')) map ConstantStr
  val constant: Parser[Constant] = (many1(digit) map ConstantInteger) | str

  val skipComb = skipMany1(char('\n') | whitespace) >| Ignore

  lazy val body = many(declaration || expression) map Body
  lazy val funBody: Parser[Body] = braces(body)

  val imp: Parser[Declaration] = (string("import") ~> w ~> id <~ `;`) map Import
  val letDecl: Parser[Declaration] = (let ~> id <~ `=`) ~ expression <~ `;` map LetDecl.tupled
  val funDecl: Parser[Declaration] = (fun ~> id ~ many(skipMany(w) ~> id) <~ `=`) ~ (expression | body) <~ `;` map { case ((a, b), c) => FunDecl(a,b,c) }
  val declaration = letDecl | funDecl | imp | skipComb

  val reference = id map Reference
  lazy val invocation = many1(expression <~ skipMany(w)) map Invocation

  val primitive = infixBuiltin map InfixBuiltin

  lazy val expression: Parser[Expression] = primitive | constant | reference | invocation

  val parser = token(many(body))*/
}
