package compiler

import cats.effect.{ExitCode, IO, IOApp}
import compiler.lexer.Reader

object Main extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = {
    Reader.tokenize("")
    IO(ExitCode.Success)
  }
}
