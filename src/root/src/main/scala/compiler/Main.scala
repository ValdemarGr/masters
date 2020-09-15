package compiler

import atto.Atto._
import atto._
import cats.effect.{Blocker, ExitCode, IO, IOApp}
import cats.implicits._
import ast.TokenCombinators

object Main extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = Blocker[IO].use{ _ =>
    val program = {
      """
        |fun add a b = a + b;
        |fun main =
        |  let c = add 1 2;
        |  c
        |""".stripMargin
    }

    val skip = Set(' ', '\n')

    val parsed = fs2.Stream(program)
      .map(x => TokenCombinators.parser parseOnly  x)
      .evalMap {
        case ParseResult.Done(rest, result) =>
          val hasOther: Boolean = rest.collectFirst{ case x if skip.contains(x) => x }.isDefined
          val o = IO.pure(result)
          val log = if (hasOther) IO(println(s"Failed with rest ${rest}")) else IO.unit
          log *> o
        case x => IO(println(s"died at $x")).as(List.empty)
      }
      .flatMap(xs => fs2.Stream(xs: _*))
      //.evalTap(x => IO(println(x)))
      .compile
      .fold(List.empty[ast.TokenTypes.Declaration]){ case (a, b) => a ++ List(b) }
    parsed.map(x => println(emitter.CEmitter.emit(x))) *> IO(ExitCode.Success)
  }
}
