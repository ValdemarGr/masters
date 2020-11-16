package compiler

import ir.LCLanguage._
import atto.Atto._
import tt.Operations._
import atto._
import cats.effect.{Blocker, ExitCode, IO, IOApp}
import cats.implicits._
import ir.LCTransform
import par._
import scala.collection.immutable.Nil
import com.codecommit.gll.Success
import com.codecommit.gll.Failure
import par.TokenTypes._
import tt.Types.Context

object Main extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = Blocker[IO].use { _ =>
    val p2 = """
               |type List a = 
               |  | Cons a (List a) 
               |  | Nil
               |;
               |
               |fun foldl f a l =
               |  match l
               |    | Nil -> a;
               |    | Cons x xs -> foldl f (f a x) xs;
               |  ;
               |
               |fun range n =
               |  if (n == 0)
               |    Nil;
               |  else
               |    Cons n (range (n - 1));
               |  ;
               |
               |fun add a b = a + b;
               |
               |type Tree a =
               |  | Tnil
               |  | Node (Tree a) a (Tree a)
               |;
               |
               |fun insert t a =
               |  match t
               |    | Tnil -> Node Tnil a Tnil;
               |    | Node l x r -> 
               |      if (x > a)
               |        Node (insert l a) x r;
               |      else
               |        Node l x (insert r a);
               |      ;      
               |  ;      
               |      
               |fun treesum t =
               |  match t
               |    | Tnil -> 0;
               |    | Node l x r -> ((treesum l) + x) + (treesum r);
               |  ;
               |
               |fun main =
               |  let t = insert (insert (insert Tnil 10) 20) 5;
               |  let b = insert t 2;
               |  let a = insert b 3;
               |  treesum t;
               //|  let b = range 10;
               //|  foldl (add) 0 (b);
               |""".stripMargin

               val simple = """
               |
               |fun main = 
               |fun f g c = g + 2;
               |let a = 22;
               |let n = f 2;
               |n;
               |
               """.stripMargin

    val parsed = par.GLLParser.parse(simple)

    val programStart = "\n\n#include <iostream>\n#include <variant>\n\nint main() {\nauto v ="
    val programEnd = ";\n\n    std::cout << v << std::endl;\n\n    return 0;\n}"

    val checked = parsed match {
      case x :: Nil => IO(x)
      case xs =>
        IO.raiseError(new Exception(s"failed parsing with ambiguity (multiple options) results \n${xs.mkString("\n\n")}"))
    }

    val succ = checked.flatMap {
      case Success(value, _) => IO.pure(value)
      case Failure(data, rest) =>
        IO.raiseError(new Exception(s"failed parsing with $data, and rest \n${rest.takeWhile(_ != ';').mkString}"))
    }

    succ.flatMap { decls =>
      //IO(println(programStart + emitter.LCEmitter.emit(LCTransform.entrypoint(decls)) + programEnd))
      //IO(println(s"(define main ${emitter.LCEmitter.emitScheme(LCTransform.entrypoint(decls))})\n(display main)"))
      IO(inferType(Context(0), Map.empty, Map.empty, decls.collectFirst{ case FunDecl(_, _, b) => b }.get)).flatMap(x => IO(println(x)))
    } *>
      //IO(println(parsed)) *>
      IO(ExitCode.Success)
  }
}
