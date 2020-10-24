package compiler

import ir.LCLanguage._
import atto.Atto._
import atto._
import cats.effect.{Blocker, ExitCode, IO, IOApp}
import cats.implicits._
import ir.LCTransform
import par._

object Main extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = Blocker[IO].use { _ =>
    println(GLLParser.parse(" \ntestVar\n"))
    val program = {
      """
        |
        |//fun f a = g a;
        |//fun g a = f a;
        |
        |//type List a = Cons a (List a) | Nil
        |
        |//type Maybe a = Just a | Nothing
        |
        |fun add a b = (a + b);
        |
        |//fun foldl f a l =
        |//  match l
        |//    | Nil -> a
        |//    | Cons x xs -> foldl (f) (f (a) (x)) xs
        |
        |//fun main =
        |//  foldl (add) 0 (Cons 2 (Cons 4 (Cons 9 Nil)))
        |
        |fun main = 
        |  (2 + 2)
        |""".stripMargin
    }

    val skip = Set(' ', '\n')

    val programStart = "\n\n#include <iostream>\n#include <variant>\n\nint main() {\nauto v ="
    val programEnd = ";\n\n    std::cout << v << std::endl;\n\n    return 0;\n}"
    val parsed = fs2
      .Stream(program)
      .map(x => TokenCombinators.parser.parseOnly(x))
      .evalMap {
        case ParseResult.Done(rest, result) =>
          val hasOther: Boolean = rest.collectFirst { case x if skip.contains(x) => x }.isDefined
          val o = IO.pure(result)
          val log =
            if (hasOther) IO.raiseError(new Exception(s"Failed with rest ${rest}"))
            else IO.unit
          log *> o
        case x => IO(println(s"died at $x")).as(List.empty)
      }
      .flatMap(xs => fs2.Stream(xs: _*))
      .compile
      .fold(List.empty[TokenTypes.Declaration]) { case (a, b) => a ++ List(b) }

    parsed
      .handleErrorWith { e =>
        e.setStackTrace(Array.empty[StackTraceElement])
        IO.raiseError(e)
      }
      .map(x => println(programStart + emitter.LCEmitter.emit(LCTransform.entrypoint(x)) + programEnd)) *>
      //.map(x => println(LCTransform.entrypoint(x).stringify(Indentation(0)))) *>
      IO(ExitCode.Success)
  }
}
