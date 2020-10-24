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
    val p2 = """
        |//abk i mat . ,, //
        |fun main =
        |  let a = add ( 4 ) 22;
        |  2 + 2;
        |""".stripMargin

    val parsed = par.GLLParser.parse(p2)

    val programStart = "\n\n#include <iostream>\n#include <variant>\n\nint main() {\nauto v ="
    val programEnd = ";\n\n    std::cout << v << std::endl;\n\n    return 0;\n}"
    //IO(println(programStart + emitter.LCEmitter.emit(LCTransform.entrypoint()) + programEnd)) *>
    IO(println(parsed)) *>
      IO(ExitCode.Success)
  }
}
