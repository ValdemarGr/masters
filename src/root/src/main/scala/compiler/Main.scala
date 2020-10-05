package compiler

import atto.Atto._
import atto._
import cats.effect.{Blocker, ExitCode, IO, IOApp}
import cats.implicits._
import ir.LCTransform
import par._

object Main extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = Blocker[IO].use{ _ =>
    val program = {
      """
        |
        |//fun f a = g a;
        |//fun g a = f a;
        |
        |type List a = Cons a (List a) | Nil
        |//
        |let lst = Nil;
        |//
        |//let snd = Cons 2 Nil
        |//
        |//fun die x xs = 2
        |//
        |//fun ele x xs = x
        |//
        |//fun main = snd (ele) 4;
        |//
        |//fun add a b = (a + b);
        |
        |fun sum a l =
        |  match l
        |    | Nil -> a
        |    | Cons x xs -> sum (a + x) xs
        |
        |fun main =
        |  sum 0 (Cons 2 (Cons 4 (Cons 9 Nil)))
        |
        |""".stripMargin
    }

    val skip = Set(' ', '\n')

    val programStart = "\n\n#include <iostream>\n\nint main() {\nauto v ="
    val programEnd = ";\n\n    std::cout << v << std::endl;\n\n    return 0;\n}"
    val parsed = fs2.Stream(program)
      .map(x => TokenCombinators.parser parseOnly x)
      .evalMap {
        case ParseResult.Done(rest, result) =>
          val hasOther: Boolean = rest.collectFirst{ case x if skip.contains(x) => x }.isDefined
          val o = IO.pure(result)
          val log =
            if (hasOther) IO.raiseError(new Exception(s"Failed with rest ${rest}"))
            else IO.unit
          log *> o
        case x => IO(println(s"died at $x")).as(List.empty)
      }
      .flatMap(xs => fs2.Stream(xs: _*))
      .compile
      .fold(List.empty[TokenTypes.Declaration]){ case (a, b) => a ++ List(b) }

    parsed
      .handleErrorWith{ e =>
        e.setStackTrace(Array.empty[StackTraceElement])
        IO.raiseError(e)
      }
      .map(x => println(programStart + emitter.LCEmitter.emit(LCTransform.entrypoint(x)) + programEnd)) *>
      IO(ExitCode.Success)
  }
}
