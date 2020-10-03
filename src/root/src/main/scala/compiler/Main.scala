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
        |fun add a b = (a + b);
        |
        |fun main = add 2 4;
        |""".stripMargin
    }

    val skip = Set(' ', '\n')

    val programStart = "#include <iostream>\n\nint main() {\nauto v ="
    val programEnd = ";\n\n    std::cout << v << std::endl;\n\n    return 0;\n}"

    val parsed = fs2.Stream(program)
      .map(x => TokenCombinators.parser parseOnly x)
      .evalMap {
        case ParseResult.Done(rest, result) =>
          val hasOther: Boolean = rest.collectFirst{ case x if skip.contains(x) => x }.isDefined
          val o = IO.pure(result)
          val log = if (hasOther) IO.raiseError {
            val e = new Exception(s"Failed with rest ${rest}")
            e.setStackTrace(Array.empty[StackTraceElement])
            e
          } else IO.unit
          log *> o
        case x => IO(println(s"died at $x")).as(List.empty)
      }
      .flatMap(xs => fs2.Stream(xs: _*))
      .compile
      .fold(List.empty[TokenTypes.Declaration]){ case (a, b) => a ++ List(b) }

    parsed.map(x => println(programStart + emitter.LCEmitter.emit(LCTransform.entrypoint(x)) + programEnd)) *>
      IO(ExitCode.Success)
  }
}
