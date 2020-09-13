package compiler.lexer

import scala.util.matching.Regex

trait Token

object Token {
  trait Match { def regex: Regex }


  trait ConstantToken extends Token
  trait TypeToken extends Token

  case class TokenBase(
                    line: Int,
                    column: Int,
                    token: String
                  )

  case class Val(t: TokenBase) extends Token
  object Val extends Match { val regex = "val".r }

  case class Colon(t: TokenBase) extends Token
  object Colon extends Match { val regex = ":".r }

  case class Int(t: TokenBase) extends TypeToken
  object Int extends Match { val regex = "Int".r }

  case class Number(t: TokenBase) extends ConstantToken
  object Number extends Match { val regex = """\d+""".r }

  val pairs: List[(Match, TokenBase => Token)] = List(
    Val -> Val.apply,
    Colon -> Colon.apply,
    Int -> Int.apply,
    Number -> Number.apply
  )
}
