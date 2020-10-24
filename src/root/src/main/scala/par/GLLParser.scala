package par

import scala.language.postfixOps
import com.codecommit.gll._
import par.TokenTypes._

object GLLParser extends Parsers with RegexParsers {
  lazy val id: Parser[String] = """[a-z][a-zA-Z]*""".r
  lazy val typeId: Parser[String] = """[A-Za-z][a-zA-Z]*""".r
  lazy val infixOp: Parser[BuiltinOperator] = (
    "+" ^^^ Addition
      | "-" ^^^ Subtraction
      | "==" ^^^ Equallity
      | "!=" ^^^ Inequallity
  )

  lazy val number: Parser[ConstantInteger] = """\d+""".r ^^ { x => ConstantInteger(x.toInt) }
  lazy val app: Parser[Expression] = (id | typeId) ~ (expr *) ^^ { (id, exps) => Apply(id, exps) }
  lazy val infix: Parser[Expression] = expr ~ infixOp ~ expr ^^ { (e1, op, e2) => InfixBuiltin(e1, op, e2) }
  lazy val conditional: Parser[Expression] =
    (
      ("if" ~> ("(" ~> expr <~ ")") ~ functionBody)
      ~ ("else" ~> functionBody)
    )
  lazy val matchCase = 
    ("|" ~> (typeId ~ (id *)) <~ "->") ~ functionBody
  lazy val patternMatch: Parser[Expression] = 
    ("match" ~> expr) ~ (matchCase *)
  lazy val expr: Parser[Expression] = number | app | infix | conditional | matchCase | patternMatch

  lazy val letD = ()
  def parse(s: String) = id(s)

}
