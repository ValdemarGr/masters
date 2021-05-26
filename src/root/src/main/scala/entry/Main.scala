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
import fs2.io._
import java.nio.file.Paths

object Main extends IOApp {
  val asScheme = "-s"

  def parse(code: String): IO[List[Declaration]] = {
    val parsed = par.GLLParser.parse(code)

    val checked = parsed match {
      case x :: Nil => IO(x)
      case xs =>
        IO.raiseError(
          new Exception(s"failed parsing with ambiguity (multiple options) results \n${xs.mkString("\n\n")}")
        )
    }

    val succ = checked.flatMap {
      case Success(value, _) => IO.pure(value)
      case Failure(data, rest) =>
        IO.raiseError(new Exception(s"failed parsing with $data, and rest \n${rest.takeWhile(_ != ';').mkString}"))
    }

    succ
  }

  def compile(code: String, asScheme: Boolean): IO[String] = {
    val succ = parse(code)

    val output = succ.map { decls =>
      //inferProgram(decls)
      val transformed = LCTransform.entrypoint(decls)
      if (asScheme)
        s"(define main ${emitter.LCEmitter.emitScheme(transformed)})\n(display main)"
      else {
        //emitter.LCEmitter.emitGraphMachine(transformed)
        //println(s"running $transformed")
        def time[R](block: => R): R = {
          val t0 = System.nanoTime()
          val result = block // call-by-name
          val t1 = System.nanoTime()
          println("Elapsed time: " + ((t1 - t0) / 1000000) + "ms")
          result
        }
        val withTrimmers = runtime.ReductionMachine.LCWithTrimmers.fromLCTerms(transformed)
        time(runtime.ReductionMachine.run(withTrimmers).toString)
      }
    }

    output
  }

  override def run(args: List[String]): IO[ExitCode] = Blocker[IO].use { b =>
    //val testProgram = """
      //| type List a = | Nil | Cons a (List a);
      //| fun sum l =
      //|   match l 
      //|     | Nil -> 0;
      //|     | Cons x xs -> x + (sum xs);
      //|   ;
      //| fun main = let l = Cons 1 Nil; sum l;
    //""".stripMargin
    //parse(testProgram)
      //.map(ir.trans.IntoLC.entrypoint)
      //.flatTap(x => IO(println(x)))
      //.map(x => runtime.ReductionMachine.run(x))
      //.flatTap(x => IO(println(x)))
      //.map(println) *> IO(ExitCode.Success)

    val files = args.map(f => file.readAll[IO](Paths.get(f), b, 1024))
    val in = fs2.Stream(files: _*).lift[IO].flatten.through(fs2.text.utf8Decode)
    val folded = in.compile.fold(""){ case (accum, next) => accum + next }
    folded
      .flatMap(parse)
      .map(ir.trans.IntoLC.entrypoint)
      .flatTap(x => IO(tt.LCTChecker.entrypoint(x)).attempt.flatMap{
        case Right((t, _, _)) => IO(println(s"completed with type $t"))
        case Left(t) => IO(println(s"failed to type with error ${t.getMessage}"))
      })
      .flatTap(x => IO(println(x)))
      .map(x => runtime.ReductionMachine.run(x))
      .flatTap(x => IO(println(x))).as(ExitCode.Success)

    //val files = args.filter(x => x != asScheme)
    ////val stdinstream = stdinUtf8[IO](1024, b)
    //val streams = files.map(f => file.readAll[IO](Paths.get(f), b, 1024))
    //val instream = fs2
      //.Stream(streams: _*)
      //.lift[IO]
      //.flatten
      //.through(fs2.text.utf8Decode)

    //val folded = instream.compile
      //.fold("") { case (accum, next) => accum + next }

    //val sch = args.find(_ == asScheme).isDefined

    //val compiledCode = folded.flatMap { code =>
      //compile(code, sch)
    //}

    //val outPipe = stdout[IO](b)

    //val written = fs2.Stream
      //.eval(compiledCode)
      //.flatMap(x => fs2.Stream(x.getBytes: _*))
      //.through(outPipe)

    //written.compile.drain
      //.as(ExitCode.Success)
  }
  /*
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
               |type Maybe a =
               |  | Nothing
               |  | Just a
               |  | Two a a
               |;
               |
               |fun main =
               |  let o = Two 2 2;
               |  let h = match o
               |    | Just n -> n;
               |    | Nothing -> 2;
               |    | Two k v -> k + v;
               |  ;
               |  h;
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
      IO(println(inferProgram(decls)))
    } *>
      //IO(println(parsed)) *>
      IO(ExitCode.Success)
  }*/
}
