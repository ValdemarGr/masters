package par

import scala.language.postfixOps
import com.codecommit.gll._
import par.TokenTypes._
import cats.data.NonEmptyList

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
  lazy val app: Parser[Expression] = (id | typeId) ~ (exprParen *) ^^ { (id, exps) => Apply(id, exps) }
  lazy val infix: Parser[Expression] = exprParen ~ infixOp ~ exprParen ^^ { (e1, op, e2) => InfixBuiltin(e1, op, e2) }
  lazy val conditional: Parser[Expression] =
    (
      ("if" ~> ("(" ~> exprParen <~ ")") ~ functionBody)
        ~ ("else" ~> functionBody)
    ) ^^ { (e, b1, b2) => If(e, b1, b2) }
  lazy val matchCase: Parser[MatchCase] =
    ("|" ~> (typeId ~ (id *)) <~ "->") ~ functionBody ^^ { (s, ps, b) => MatchCase(s, ps, b) }
  lazy val patternMatch: Parser[Expression] =
    ("match" ~> exprParen) ~ (matchCase +) ^^ { (e, cs) => PatternMatch(e, NonEmptyList(cs.head, cs.tail)) }
  lazy val expr: Parser[Expression] = number | app | infix | conditional | patternMatch
  lazy val exprParen: Parser[Expression] = ("(" ~> expr <~ ")") | expr

  lazy val letD: Parser[ValueDeclaration] =
    ("let" ~> id <~ "=") ~ exprParen <~ ";" ^^ { (id, e) => LetDecl(id, e) }
  lazy val funD: Parser[ValueDeclaration] =
    ("fun" ~> id ~ (id *) <~ "=") ~ functionBody ^^ { (id, ps, fb) => FunDecl(id, ps.map(FunctionParam), fb) }
  lazy val declaration: Parser[ValueDeclaration] = letD | funD
  lazy val comment = "//" ~> """.*""".r ^^^ Ignore

  lazy val functionBody =
    ((
      declaration
        | comment
    ) *) ~ exprParen <~ ";" ^^ { (b, e) => FunctionBody(b, e) }
  
  lazy val typeName: Parser[TypeName] = id ^^ TypeName
  lazy val parensType: Parser[TypeParam] = "(" ~> tagType <~ ")" ^^ ParensType
  lazy val typeParams: Parser[List[TypeParam]] = (typeName | parensType)* 
  lazy val tagType: Parser[TagType] = 
    typeId ~ typeParams ^^ TagType
  lazy val disjointUnion =
    ("|" ?) ~> tagType ~ (("|" ~> tagType)*) ^^ NonEmptyList.apply ^^ DisjointUnion
  lazy val typeDecl: Parser[TypeDeclaration] =
    ("type" ~> (typeId ~ (id*)) <~ "=") ~ disjointUnion ^^ TypeDeclaration
    lazy val typelevelDecl = typeDecl <~ ";"
  

  lazy val toplevel: Parser[List[Declaration]] = (typelevelDecl | declaration | comment) *
  def parse(s: String) = toplevel(s).toList

}
