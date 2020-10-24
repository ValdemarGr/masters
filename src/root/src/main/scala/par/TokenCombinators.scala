package par

import atto._
import Atto._
import cats.data.NonEmptyList

object TokenCombinators {
  /*import TokenTypes._
  implicit class ParserTap[+A](p: Parser[A]) {
    def tap(f: A => Unit): Parser[A] = p.map { value =>
      f(value)
      value
    }

    def ptap(name: String): Parser[A] = tap{ value =>
      //println(s"in ${name} got value ${value}")
      val _ = (name, value)
    }
  }

  def spaces[A](p: => Parser[A]): Parser[A] = many(spaceChar) ~> p <~ many(spaceChar)
  val endDecl = spaces(char('\n').ptap("end nl") | char(';').ptap("end ;"))
  val let = string("let") ~> many(spaceChar)
  val fun = string("fun") ~> many(spaceChar)
  def genericId(fst: Parser[Char]) = spaces(fst ~ many(letter | char('_'))) map { case (first, rest) => NonEmptyList(first, rest) }
  val id = genericId(lower).ptap("id")
  val typeId = genericId(upper).ptap("typeid")
  val infixBuiltin: Parser[BuiltinOperator] = {
    val addition = char('+') >| Addition
    val subtraction = char('-') >| Subtraction
    val eq = string("==") >| Equallity
    val ineq = string("!=") >| Inequallity
    addition | subtraction | eq | ineq
  }
  val `=` = spaces(char('='))
  val `type` = string("type") ~> many(spaceChar)
  val idParams = id ~ many(id)

  def excludeText(exclude: Char) = many(elem(_ != exclude))

  def t3[A, B, C](t: ((A, B), C)): (A, B, C) = t match { case ((a, b), c) => (a, b, c) }

  val number: Parser[Expression] = spaces(int.ptap("int")) map ConstantInteger
  val app: Parser[Expression] =
    spaces((id | typeId).ptap("app id")) ~ spaces(many(expression.ptap("app expr"))) map Apply.tupled
  def infixGeneric(p: Parser[Expression]): Parser[Expression] =
    spaces(p.ptap("infix lhs")) ~ spaces(infixBuiltin.ptap("infix")) ~ spaces(p.ptap("infix rhs")) map t3 map InfixBuiltin.tupled
  def infix: Parser[Expression] = parens(infixGeneric(expression))
  def conditional: Parser[Expression] =
    (spaces(string("if")) ~> many(spaceChar | char('\n')) ~> spaces(expression) <~ many(spaceChar | char('\n'))) ~
    ((functionBody | (expression map FunctionBody.curried(Nil))) <~ spaces(string("else")) <~ many(spaceChar | char('\n'))) ~
    functionBody map t3 map If.tupled
  def matchCase =
    (spaces(char('|')) ~> (typeId ~ many(id)) <~ spaces(string("->"))) ~ functionBody map t3 map MatchCase.tupled
  def patternMatch: Parser[Expression] = (spaces(string("match")) ~> expression) ~ (spaces(char('\n')) ~> many1(spaces(matchCase))) map PatternMatch.tupled
  val expression: Parser[Expression] = spaces((number | infix | conditional | patternMatch | parens(app) | app).ptap("parens"))

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
  val tagType = typeId.ptap("tag name") ~ typeParams.ptap("tag params") <~ many(spaceChar) map TagType.tupled
  val disjointUnion =
    tagType ~ many(char('|') ~> tagType) map { case (x, xs) => DisjointUnion(NonEmptyList(x, xs)) }
  val typeDecl: Parser[TypeDeclaration] =
    (`type` ~> (typeId ~ many(id)) <~ `=`) ~ disjointUnion map t3 map TypeDeclaration.tupled
  val typelevelDecl = typeDecl <~ endDecl

  def functionBody = (many(
        declaration.ptap("decl")
      | redundantNl.ptap("redundant nl")
      | (spaceChar >| Ignore).ptap("space")
      | comment.ptap("comment")
  ) ~ (expression.ptap("fun expr") <~ endDecl.ptap("end fun decl"))) map FunctionBody.tupled

  val body: Parser[List[Declaration]] = many(declaration | typelevelDecl | redundantNl | comment)

  val parser: Parser[List[Declaration]] = token(body)*/
}
