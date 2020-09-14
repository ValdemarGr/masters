package compiler

import atto.Atto._
import atto._
import cats.effect.{Blocker, ExitCode, IO, IOApp}
import cats.implicits._
import tokens.TokenCombinators

object Main extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = Blocker[IO].use{ _ =>
    //val imp = "import std;"
    //val let = "let a = 22;"
    val program = {
      """
        |import std;
        | //as
        |let a = 22
        |
        |fun a b c = 22
        |fun a =
        |   let b = 22
        |   let c = 44
        |   44
        |;
        |let c = 'a';
        |""".stripMargin
    }

    val parsed = fs2.Stream(program)
      .map(x => TokenCombinators.parser parseOnly x)
      .evalMap {
        case ParseResult.Done(_, result) => IO.pure(result)
        case x => IO(println(s"died at $x")).as(List.empty)
      }
      .flatMap(xs => fs2.Stream(xs: _*))
      .evalTap(x => IO(println(x)))
      .compile
      .drain
    parsed *> IO(ExitCode.Success)
  }
}
