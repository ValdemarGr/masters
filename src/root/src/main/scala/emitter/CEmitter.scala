package emitter

import cats.data.NonEmptyList

object CEmitter {
  import ast.TokenTypes._
  implicit def str(nel: NonEmptyList[Char]): String = (nel.head :: nel.tail).mkString

  def emitExpr(expr: Expression): String = expr match {
    case InfixBuiltin(lhs, op, rhs) => s"(${emitExpr(lhs)} ${op} ${emitExpr(rhs)})"
    case FunctionBody(children, end) => children.map(emitDecl).mkString("\n") + s"\nreturn ${emitExpr(end)};"
    case Apply(id, params) => str(id) + (if (params.isEmpty) "" else s"(${params.map(emitExpr).mkString(", ")})")
    case ConstantInteger(number) => str(number)
    case ConstantStr(_) => ???
  }

  def emitDecl(decl: Declaration): String = decl match {
    case Ignore => ""
    case FunDecl(id, params, body) => {
      val bodyStr = emitExpr(body.fold(identity, identity))
      s"""int ${str(id)}(${params.map(p => s"int ${str(p)}").mkString(", ")}) {
         | ${bodyStr}
         |}
         |""".stripMargin
    }
    case LetDecl(id, expr) => s"int ${str(id)} = ${emitExpr(expr)};"
    case _ => ???
  }

  def emit(body: List[Declaration]): String = body.foldLeft(""){ case (accum, next) =>
    accum + emitDecl(next)
  }
}
